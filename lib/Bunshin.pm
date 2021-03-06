
=head1 NAME

Bunshin --- A shimbun implemrntion written in Perl

=cut

package Bunshin;
use strict;
use vars qw($DEBUG $MYNAME $VERSION);
$VERSION=do{my @r=(q$Revision: 1.6 $=~/\d+/g);sprintf "%d."."%02d" x $#r,@r};
$MYNAME = 'Bunshin';
$DEBUG = 0;
use FileHandle;
require Message::Entity;
require Message::Util;
require Message::Field::Date;

sub new ($;%) {
  my $class = shift;
  my $self = bless {}, $class;
  $self->{fmt2str} = Message::Util::make_clone
    (\%Message::Field::Date::FMT2STR);
  $self;
}

=item $b->msg_regex ($regex)

Set regex used to cut a message.

=cut

sub set_regex ($$$) {
  my $self = shift;
  my $name = shift;
  my $regex = shift;
  $regex =~ s/\x20/\\x20/g;
  $regex =~ s/\x09/\\x09/g;
  $regex =~ s/\x0D(?!\x0A)/\x0D\x0A/g;
  $regex =~ s/(?<!\x0D)\x0A/\x0D\x0A/g;
  $regex =~ s/\x0D/\\x0D/g;
  $regex =~ s/\x0A/\\x0A/g;
  $regex =~ s/\x23/\\x23/g;
  $self->{'regex_'.$name} = $regex;
}

sub set_hook_function ($$\&) {
  my $self = shift;
  my $name = shift;
  my $function = shift;
  $self->{'hook_'.$name} = $function;
}

sub set_element_decoders ($%) {
  my $self = shift;
  my %list = @_;
  for (keys %list) {
    if ($list{$_} eq 'enentity_html') {
      $list{$_} = \&Message::Util::enentity_html;
    } elsif ($list{$_} eq 'deentity_html') {
      $list{$_} = \&Message::Util::deentity_html;
    ## TODO: escape_uri, unescape_uri
    }
  }
  $self->{element_decoder} = %list;
}

sub set_format ($$\&) {
  my $self = shift;
  my $name = shift;
  my $function = shift;
  $self->{fmt2str}->{$name} = $function;
}

sub set_elements ($$@) {
  my $self = shift;
  my $name = shift;
  $self->{'elements_'.$name} = \@_;
}

sub set_source ($%) {
  my $self = shift;
  my %option = @_;
  ($self->{source}, $self->{meta_info}) = $self->_get_resource (%option);
  $self->default_parameter (base_uri => $option{uri}) if $option{uri};
  	## BUG: Doesn't support redirection
  if ($option{uri} || $option{file}) {
    my $c = $self->{hook_code_conversion} || \&_code_conversion;
    $self->{source} = &$c ($self, $self->{source}, \%option, $self->{meta_info});
  }
  $self->{source} =~ s/\x0D(?!\x0A)/\x0D\x0A/g;
  $self->{source} =~ s/(?<!\x0D)\x0A/\x0D\x0A/g;
  $self;
}

sub _get_resource ($%) {
  my $self = shift;
  my %option = @_;
  my ($resource, $meta);
  if (defined $option{value}) {
    $resource = $option{value};
  } elsif ($option{uri}) {
    require Message::Field::UA;
    require LWP::UserAgent;
    my $ua = Message::Field::UA->new;
    $ua->add_our_name;
    $ua->add ('libwww-perl' => $LWP::VERSION);
    my $lwp = LWP::UserAgent->new;
    $lwp->agent ($ua->stringify);
    my $req = HTTP::Request->new (GET => $option{uri});
    my $res = $lwp->request ($req);
    $resource = $res->content;
    $meta = parse Message::Header $res->headers_as_string,
      -parse_all => 0, -format => 'http-response',
    ;
  } elsif ($option{file}) {
    my $f = new FileHandle $option{file} => 'r';
    Carp::croak "set_source: $option{file}: $!" unless defined $f;
    my $c = $self->{hook_code_conversion} || \&_code_conversion;
    local $/ = undef;
    $resource = $f->getline;
  } else {
    Carp::croak "_get_resource: $_[0]: Unsupported data source type";
  }
  ($resource, $meta);
}

## $self->_code_conversion ($string, \%option, $meta_info)
sub _code_conversion ($$\%$) {
  $_[1];
}

sub make_msgs ($) {
  my $self = shift;
  my $s = $self->{source};
  my $f = $self->{hook_make_msg} || \&_make_a_msg;
  my @msg;
  my %param = %{$self->{default_parameter}};
  if ($self->{regex_metainfo} && ref $self->{elements_metainfo}) {
    $s =~ s{ $self->{regex_metainfo} }{
      no strict 'refs';
      for my $i (0..$#{$self->{elements_metainfo}}) {
        $param{$self->{elements_metainfo}->[$i]} = ${$i+1};
      }
      $&;
    }esx;
  }
  $s =~ s{ $self->{regex_message} }{
    no strict 'refs';
    my %p = %param;
    for my $i (0..$#{$self->{elements_message}}) {
      $p{$self->{elements_message}->[$i]} = ${$i+1};
    }
    for my $n (keys %{$self->{element_decoder}}) {
      if ($p{$n} && ref $self->{element_decoder}->{$n}) {
        $p{$n} = &{ $self->{element_decoder}->{$n} } ($p{$n});
      }
    }
    my $msg = &$f ($self, %p);
    push @msg, $msg;
  }gesx;
  @msg;
}

## Default function for "make_msg"
sub _make_a_msg ($@) {
  my $self = shift;
  my %p = @_;
  my $msg = new Message::Entity
    -fill_date	=> 0,
    -fill_msgid	=> 0,
    -fill_ua_name	=> 'x-shimbun-agent',
    -format	=> 'news-usefor',
    -parse_all	=> 1,
  ;
  my $hdr = $msg->header;
  ## Originator and date
    my $from = $hdr->field ('from')->add ($p{from_mail} || 'foo@bar.invalid');
    $from->display_name ($p{from_name}) if length $p{from_name};
    my $date = $hdr->field ('date');
    $p{date_year} ||= (gmtime)[5];
    $date->set_datetime (@p{qw/date_year date_month
      date_day date_hour date_minute date_second/},
      zone => $p{date_zone});
    $hdr->add (x_uri => $p{from_uri}) if $p{from_uri};
    if ($p{from_face}) {
      $msg->header->field ('x-face')->value ($p{from_face});
    } elsif ($p{faces}->{$p{from_mail}}) {
      $msg->header->field ('x-face')->value ($p{faces}->{$p{from_mail}});
    } elsif ($p{list_face}) {
      $msg->header->field ('x-face')->value ($p{list_face});
    }
  ## Message attribute
    if (length $p{msg_id}) {
      $hdr->add ('message-id' => $p{msg_id});
    } elsif ($p{msg_id_from} || $p{msg_id_right} || $p{list_id}) {
      my $c = $p{msg_count};
      $c = '.d'.(0+$date) unless defined $p{msg_count};
      my $mid;
      if ($p{msg_id_from}) {
        $mid = sprintf '<msg%s.BS%%%s%%%s>', $c, $p{list_id}, $p{msg_id_from};
      } elsif ($p{msg_id_right}) {
        $mid = sprintf '<msg%s.BS%%%s%%list@%s>', $c, $p{list_id}, $p{msg_id_right};
      } else { #if ($p{list_id}) {
        $mid = sprintf '<msg%s.BS%%list@%s>', $c, $p{list_id};
      }
      $hdr->add (($DEBUG?'x-':'').'message-id' => $mid);
    }
    if (length $p{subject}) {
      $hdr->add (subject => $p{subject});
    } elsif (length $p{DEFAULT_subject}) {
      $hdr->add (subject => $p{DEFAULT_subject});
    }
    my $a;
    for (grep {/^misc_/} keys %p) {
      $a = $hdr->field ('content-x-properties') unless ref $a;
      my $name = substr ($_, 5);
      $name =~ tr/_/-/;
      if ($p{base_uri} && /uri/ && length $p{$_}) {
        require URI::WithBase;
        $a->add ($name => URI::WithBase->new ($p{$_}, $p{base_uri})->abs);
      } elsif (/color/) {
        $p{$_} = '#'.$p{$_} if $p{$_} =~ /^[A-Za-z0-9]{6}$/;
        $a->add ($name => $p{$_}) if length $p{$_};
      } else {
        $a->add ($name => $p{$_}) if length $p{$_};
      }
    }
  ## Body and body information
    my $b = $self->{hook_msg_body} || \&_make_a_msg_body;
    &$b ($self, $msg, $p{body}, \%p);
  ## List information
    if (length $p{list_id}) {
      my $lid = $hdr->field ('list-id');
      $lid->value ($p{list_id});
      $lid->display_name ($p{list_name}) if length $p{list_name};
    }
    $hdr->add (x_mail_count => $p{msg_count}) if defined $p{msg_count};
    $hdr->add (x_ml_info => $p{list_info}) if defined $p{list_info};
    if ($p{base_uri}) {
      my $uri = $hdr->add (x_uri => '');
      $uri->value ($p{base_uri});
      $uri->display_name ($p{list_name}) if length $p{list_name};
    }
    if ($p{urn_template}) {
      my $urn = $date->stringify (
      	-format_macros	=> $self->{fmt2str},
      	-format_template	=> $p{urn_template},
      	-format_parameters	=> \%p,
      );
      #my $urn = $self->Message::Field::Date::_date2str ({
      #  format_template	=> $p{urn_template},
      #  date_time	=> $date->unix_time,
      #  zone	=> $date->zone,
      #  fmt2str	=> $self->{fmt2str},
      #}, \%p);
      $hdr->add ('x-uri')->value ($urn);
    }
  ## Additional information
    my $u = $self->{hook_msg_header_add};
    &$u ($self, $msg, \%p) if ref $u;
    $hdr->field ('x-shimbun-agent')->add ($MYNAME => $VERSION);
  $msg;
}

sub _make_a_msg_body ($$$\%) {
  my $self = shift;
  my ($msg, $body, $param) = @_;
  if (length $body) {
    $body =~ s/(?<!\x0D\x0A)\z/\x0D\x0A/s;
    $msg->body ($body);
  }
}

sub default_parameter ($@) {
  my $self = shift;
  if (@_ == 1) {
    return $self->{default_parameter}->{ $_[0] };
  }
  while (my ($name, $value) = splice (@_, 0, 2)) {
    $self->{default_parameter}->{$name} = $value;
  }
  $self;
}

=head1 $meta = $b->meta_information

Gets meta information from resource requesting protocol.
Usually returned value is Message::Header object.
When resource is getten via HTTP, its content is HTTP response
header.

=cut

sub meta_information ($) {
  my $self = shift;
  $self->{meta_info};
}

=head1 LICENSE

Copyright 2002 wakaba E<lt>w@suika.fam.cxE<gt>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

=head1 CHANGE

See F<ChangeLog>.
$Date: 2002/09/10 23:37:43 $

=cut

1;
