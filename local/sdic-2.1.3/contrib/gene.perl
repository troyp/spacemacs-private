#!/usr/local/bin/perl

# Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
# Keywords: dictionary

# GENE95������Ѵ����� Perl ������ץ�

# GENE95����ϡ�Kurumi ���� Nifty-Serve �Ѳ��åե������Ǹ��������
# ������¼���Ǥ���������Ρפ� SDIC �Υڡ�����������Ǥ��ޤ���
#
#     http://pine.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/gene.html
#
# COMPAT�������Ѵ�������ϡ�
#
#     nkf -S -e gene.txt | perl gene.perl --compat >gene.dic
#
# SDIC�������Ѵ�������ϡ�
#
#     nkf -S -e gene.txt | perl gene.perl >gene.sdic
#
# �Ȼ��ꤷ�Ʋ����������줾��η����ξܺ٤ˤĤ��Ƥ� sdic.texi �򻲾ȡ�
#
# �ʤ���COMPAT�����μ����SDIC�������Ѵ�������ϡ�
#
#    perl gene.perl --compat-to-sdic gene.dic >gene.sdic
#
# �Ȥ��Ʋ�������
#
# SDIC�����μ����COMPAT�������Ѵ�������ϡ�
#
#    perl gene.perl --sdic-to-compat gene.sdic >gene.dic
#
# �Ȥ��Ʋ�������SDIC�����Τۤ�������˭���ʤΤǡ������Ѵ���Ԥ��Ȱ���
# Ū�ˤϾ��󤬷���ޤ��Τǡ���դ��ƻȤäƲ�������


eval { binmode(STDOUT); };

if(( $ARGV[0] eq '--compat' )){
    shift;
    &compat();
} elsif(( $ARGV[0] eq '--compat-to-sdic' )){
    shift;
    &compat_to_sdic();
} elsif(( $ARGV[0] eq '--sdic-to-compat' )){
    shift;
    &sdic_to_compat();
} else {
    &sdic();
}

# SDIC�����μ����Ĥ���ؿ�
sub sdic {
    $_ = <>;				# 2���ɤ����Ф�
    s/\s*$/\n/;				# ���ԥ����ɤ��Ѵ� [sdic:00428]
    print "# ",$_;
    $_ = <>;
    s/\s*$/\n/;
    print "# ",$_;
    for( $i=0; <>; $i++ ){
	s/\s+$//;			# �����ζ���ʸ������
	s/&/&amp;/g;			# �᥿����饯�����ִ�����
	s/</&lt;/g;
	s/>/&gt;/g;
	if( $i%2==0 ){
	    $key = $_;
	}else{
	    $head = $key;
	    $key  =~ tr/A-Z/a-z/;
	    $key  =~ s/\s+/ /;
	    $key  =~ s/ \+\d+//;
	    if( $key eq $head ){
		push( @line, "$key\x00$head\x00<<K>$key</K>$_\n" );
	    } else {
		push( @line, "$key\x00$head\x00<<H>$head</H><K>$key</K>$_\n" );
	    }
	}
    }
    for( sort @line ){
	@f = split(/</,$_,2);
	print $f[1];
    }
}

# COMPAT�����μ������ؿ�
sub compat {
    <>;					# 2���ɤ����Ф�
    <>;
    for( $i=0; <>; $i++ ){
	s/\s+$//;
	s/\t/        /g;
	if( $i%2==0 ){
	    $word = $_;
	}else{
	    $orig = $word;
	    $word =~ tr/A-Z/a-z/;
	    push( @line, "$word\x00$orig\x00\t$orig\t$_\n" );
	}
    }
    for( sort @line ){
	@f = split(/\t/,$_,3);
	print "$f[1]\t$f[2]";
    }
}

# COMPAT�����μ����SDIC�������Ѵ�����ؿ�
sub compat_to_sdic {
    while( <> ){
	s/\s+$//;			# �����ζ���ʸ������
	s/&/&amp;/g;			# �᥿����饯�����ִ�����
	s/</&lt;/g;
	s/>/&gt;/g;
	@f = split( /\t/,$_,2 );
	$k = $f[0];
	$k =~ tr/A-Z/a-z/;
	$k =~ s/\s+/ /;
	$k =~ s/ \+\d+//;
	if( $k eq $f[0] ){
	    push( @line, "$k\x00$f[0]\x00<<K>$k</K>$f[1]\n" );
	} else {
	    push( @line, "$k\x00$f[0]\x00<<H>$f[0]</H><K>$k</K>$f[1]\n" );
	}
    }
    for( sort @line ){
	@f = split(/</,$_,2);
	print $f[1];
    }
}

# SDIC�����μ����COMPAT�������Ѵ�����ؿ�
#     SDIC�����Τۤ�������¿�����ᡢCOMPAT�����ˤ���ȡ��ɤ����Ƥ��
#     ��η�������뤿�ᡢ��դ������Ѥ��Ƥ���������
sub sdic_to_compat {
    while( <> ){
	next unless /^</;
	s/\s+$//;			# �����ζ���ʸ������
	s!^<([KH])>(.*?)</\1>!!;	# ���Ф������Ф�
	$head = $2;
	$head =~ s/&lt;/</g;		# ���Ф���Υ᥿����饯�����ִ�����
	$head =~ s/&gt;/>/g;
	$head =~ s/&amp;/&/g;
	while( s!^<K>(.*)</K>!! ){ ; }
	s/&lt;/</g;			# ����ʸ�Υ᥿����饯�����ִ�����
	s/&gt;/>/g;
	s/&amp;/&/g;
	s/\t/        /g;
	$key = $head;
	$key  =~ tr/A-Z/a-z/;
	push( @line, "$key\x00$head\x00\t$head\t$_\n" );
    }
    for( sort @line ){
	@f = split(/\t/,$_,3);
	print "$f[1]\t$f[2]";
    }
}
