#!/usr/bin/perl
use strict;
use warnings;

my %variables;


use constant {
    TOKEN_NUMBER     => 0,
    TOKEN_IDENTIFIER => 1,
    TOKEN_PLUS       => 2,
    TOKEN_MINUS      => 3,
    TOKEN_MULTIPLY   => 4,
    TOKEN_DIVIDE     => 5,
    TOKEN_EQUALS     => 6,
    TOKEN_LPAREN     => 7,
    TOKEN_RPAREN     => 8,
    TOKEN_EOF        => 9
};


sub new_token {
    my ($type, $value) = @_;
    return {
        type  => $type,
        value => $value
    };
}


package Lexer {
    sub new {
        my ($class, $text) = @_;
        my $self = {
            text     => $text,
            pos      => 0,
            curr_char => substr($text, 0, 1) || ''
        };
        return bless $self, $class;
    }

    sub advance {
        my $self = shift;
        $self->{pos}++;
        if ($self->{pos} < length($self->{text})) {
            $self->{curr_char} = substr($self->{text}, $self->{pos}, 1);
        } else {
            $self->{curr_char} = '';
        }
    }

    sub skip_whitespace {
        my $self = shift;
        while ($self->{curr_char} && $self->{curr_char} =~ /\s/) {
            $self->advance();
        }
    }

    sub number {
        my $self = shift;
        my $result = '';
        my $has_decimal = 0;

        while ($self->{curr_char} && ($self->{curr_char} =~ /\d/ || ($self->{curr_char} eq '.' && !$has_decimal))) {
            if ($self->{curr_char} eq '.') {
                $has_decimal = 1;
            }
            $result .= $self->{curr_char};
            $self->advance();
        }

        return $result;
    }

    sub identifier {
        my $self = shift;
        my $result = '';

        while ($self->{curr_char} && ($self->{curr_char} =~ /\w/)) {
            $result .= $self->{curr_char};
            $self->advance();
        }

        return $result;
    }

    sub get_next_token {
        my $self = shift;

        while ($self->{curr_char}) {
            # Skip whitespace
            if ($self->{curr_char} =~ /\s/) {
                $self->skip_whitespace();
                next;
            }

            # Numbers
            if ($self->{curr_char} =~ /\d/) {
                return new_token(TOKEN_NUMBER, $self->number());
            }

            # Identifiers
            if ($self->{curr_char} =~ /[a-zA-Z]/) {
                return new_token(TOKEN_IDENTIFIER, $self->identifier());
            }

            # Operators
            if ($self->{curr_char} eq '+') {
                $self->advance();
                return new_token(TOKEN_PLUS, '+');
            }

            if ($self->{curr_char} eq '-') {
                $self->advance();
                return new_token(TOKEN_MINUS, '-');
            }

            if ($self->{curr_char} eq '*') {
                $self->advance();
                return new_token(TOKEN_MULTIPLY, '*');
            }

            if ($self->{curr_char} eq '/') {
                $self->advance();
                return new_token(TOKEN_DIVIDE, '/');
            }

            if ($self->{curr_char} eq '=') {
                $self->advance();
                return new_token(TOKEN_EQUALS, '=');
            }

            if ($self->{curr_char} eq '(') {
                $self->advance();
                return new_token(TOKEN_LPAREN, '(');
            }

            if ($self->{curr_char} eq ')') {
                $self->advance();
                return new_token(TOKEN_RPAREN, ')');
            }

            # Unexpected character
            die "Error: Invalid character: " . $self->{curr_char};
        }

        return new_token(TOKEN_EOF, undef);
    }
}


package Parser {
    sub new {
        my ($class, $lexer) = @_;
        my $self = {
            lexer => $lexer,
            current_token => $lexer->get_next_token()
        };
        return bless $self, $class;
    }

    sub error {
        my ($self, $message) = @_;
        die "Error: $message";
    }

    sub eat {
        my ($self, $token_type) = @_;
        if ($self->{current_token}->{type} == $token_type) {
            $self->{current_token} = $self->{lexer}->get_next_token();
        } else {
            $self->error("Unexpected token: " . $self->{current_token}->{value});
        }
    }

    sub factor {
        my $self = shift;
        my $token = $self->{current_token};

        if ($token->{type} == TOKEN_NUMBER) {
            $self->eat(TOKEN_NUMBER);
            return $token->{value};
        } elsif ($token->{type} == TOKEN_IDENTIFIER) {
            my $var_name = $token->{value};
            $self->eat(TOKEN_IDENTIFIER);
            
            if (!exists $::variables{$var_name}) {
                $self->error("Undefined variable: $var_name");
            }
            return $::variables{$var_name};
        } elsif ($token->{type} == TOKEN_LPAREN) {
            $self->eat(TOKEN_LPAREN);
            my $result = $self->expr();
            $self->eat(TOKEN_RPAREN);
            return $result;
        } else {
            $self->error("Invalid syntax");
        }
    }

    sub term {
        my $self = shift;
        my $result = $self->factor();

        while ($self->{current_token}->{type} == TOKEN_MULTIPLY || 
               $self->{current_token}->{type} == TOKEN_DIVIDE) {
            my $token = $self->{current_token};
            
            if ($token->{type} == TOKEN_MULTIPLY) {
                $self->eat(TOKEN_MULTIPLY);
                $result *= $self->factor();
            } elsif ($token->{type} == TOKEN_DIVIDE) {
                $self->eat(TOKEN_DIVIDE);
                my $divisor = $self->factor();
                if ($divisor == 0) {
                    $self->error("Division by zero");
                }
                $result /= $divisor;
            }
        }

        return $result;
    }

    sub expr {
        my $self = shift;
        
 
        if ($self->{current_token}->{type} == TOKEN_IDENTIFIER) {
            my $var_name = $self->{current_token}->{value};
            $self->eat(TOKEN_IDENTIFIER);
            
            if ($self->{current_token}->{type} == TOKEN_EQUALS) {
                $self->eat(TOKEN_EQUALS);
                my $value = $self->expr();
                $::variables{$var_name} = $value;
                return $value;
            } else {
    
                $self->{lexer} = Lexer->new($var_name . substr($self->{lexer}->{text}, $self->{lexer}->{pos}));
                $self->{current_token} = $self->{lexer}->get_next_token();
            }
        }
        
        my $result = $self->term();

        while ($self->{current_token}->{type} == TOKEN_PLUS || 
               $self->{current_token}->{type} == TOKEN_MINUS) {
            my $token = $self->{current_token};
            
            if ($token->{type} == TOKEN_PLUS) {
                $self->eat(TOKEN_PLUS);
                $result += $self->term();
            } elsif ($token->{type} == TOKEN_MINUS) {
                $self->eat(TOKEN_MINUS);
                $result -= $self->term();
            }
        }

        return $result;
    }

    sub parse {
        my $self = shift;
        return $self->expr();
    }
}


print "Perl Calculator Interpreter\n";
print "Enter expressions or 'quit'/'exit' to quit\n";

while (1) {
    print "> ";
    my $input = <STDIN>;
    chomp $input;
    
    last if $input =~ /^(quit|exit)$/i;
    next if $input =~ /^\s*$/;
    
    eval {
        my $lexer = Lexer->new($input);
        my $parser = Parser->new($lexer);
        my $result = $parser->parse();
        print "$result\n";
    };
    if ($@) {
        print "$@\n";
    }
} 