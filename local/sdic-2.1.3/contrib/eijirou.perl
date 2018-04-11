#!/usr/local/bin/perl

# Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
# Keywords: dictionary

# �رѼ�Ϻ�٤��Ѵ����� Perl ������ץ�

# �Ѽ�Ϻ�ϡ����˵���ʥƥ����ȥ١����μ���Ǥ�����������Internet ��
# �Ǥ����ۤ���Ƥ��餺��Nifty-Serve �����������ɤ��뤫��CD-ROM ��
# ���Ͻ��Ҥ��������ɬ�פ�����ޤ���
#
# COMPAT�������Ѵ�������ϡ�
#
#     nkf -S -e [file]... | perl eijirou.perl --compat >eijirou.dic
#
# SDIC�������Ѵ�������ϡ�
#
#     nkf -S -e [file]... | perl eijirou.perl >eijirou.sdic
#
# �Ȼ��ꤷ�Ʋ����������줾��η����ξܺ٤ˤĤ��Ƥ� sdic.texi �򻲾ȡ�

$CHAR = "(?:[\xa1-\xfe][\xa1-\xfe]|[^\xa1-\xfe])";
$HIRAGANA = "(?:[\xa4][\xa1-\xf3])";

for( @ARGV ){
    if( $_ eq '--unsort' ){
	$UNSORT = 1;
    } elsif( $_ eq '--compat' ){
	$COMPAT = 1;
    } elsif( $_ eq '--waei' ){
	$WAEI = 1;
    } else {
	push( @tmp, $_ );
    }
}
@ARGV = @tmp;

eval { binmode(STDOUT); };

if( $COMPAT ){
    &compat();
} else {
    &sdic();
}

# COMPAT�����μ������������ؿ�
sub compat {
    while( <> ){
	s/\s+$//;			# �����β��ԥ����ɤ���
	s/\t/        /g;		# ���֤����8ʸ�����ִ�
	s/^[\200-\377]{2}//;		# ��Ƭ������ʸ������
	s/(\{[^\}]+\}) : / : $1 /;	# {��} ������ʸ�˰�ư����
	( $key,$content ) = split( / +: /,$_,2 );
	$head = $key;
	$key =~ tr/A-Z/a-z/;
	if( $UNSORT ){
	    print "$head\t$content\n";
	} else {
	    push( @line, "$key\x00$head\x00\t$head\t$content\n" );
	}
    }
    unless( $UNSORT ){
	for( sort @line ){
	    @f = split(/\t/,$_,3);
	    print "$f[1]\t$f[2]";
	}
    }
}

# SDIC �����μ������������ؿ�
sub sdic {
    while( <> ){
	s/\s+$//;			# �����β��ԥ����ɤ���
	s/&/&amp;/g;			# �᥿����饯�����ִ�����
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^[\200-\377]{2}//;		# ��Ƭ������ʸ������
	( $head,$content ) = split( / +: /,$_,2 );
	$key = $head;
	$key =~ s/ +\{[^\}]+\}//;	# {��} �򸡺���������������
	$key =~ s/ +\(\d+\)//;		# (��) �򸡺���������������
	$key =~ tr/A-Z/a-z/;
	$key =~ s/\s+/ /;
	if( $WAEI ){
	    # �±Ѽ�ϯ����ͭ��Ĵ����Ԥ�
	    while( $content =~ s/^($CHAR*?)��/$1 \/ /o ){ ; }
	    $key =~ s/^($CHAR*?)��(?:��$CHAR*?��|��)$HIRAGANA?$/$1/o;
	    $key =~ s/^($CHAR*?)��$CHAR*?$/$1/o;
	}
	if( $UNSORT ){
	    if( $key eq $head ){
		print "<K>$key</K>$content\n";
	    } else {
		print "<H>$head</H><K>$key</K>$content\n";
	    }
	} else {
	    if( $key eq $head ){
		push( @line,"$key\x00$head\x00<<K>$key</K>$content\n" );
	    } else {
		push( @line,"$key\x00$head\x00<<H>$head</H><K>$key</K>$content\n" );
	    }
	}
    }
    unless( $UNSORT ){
	for( sort @line ){
	    @f = split(/</,$_,2);
	    print $f[1];
	}
    }
}
