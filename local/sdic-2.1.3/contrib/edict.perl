#!/usr/local/bin/perl

# Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
# Keywords: dictionary

# EDICT�����μ����SDIC�������Ѵ����� Perl ������ץ�

# EDICT�����ϡ��������ȥ�ꥢ��Monash��ؤ� Jim Breen ���������������
# �����±Ѽ���Υե����ޥåȤǤ������Υե����ޥåȤμ���ϡ��ʲ���URL
# ��������Ǥ��ޤ���
#
#     ftp://ftp.u-aizu.ac.jp/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
#
# SDIC�������Ѵ�������ϡ����Τ褦�����Ѥ��Ʋ�������
#
#     perl edict.perl edict >edict.sdic
#
# SDIC�����ξܺ٤ˤĤ��Ƥ� sdic.texi �򻲾Ȥ��Ʋ����������¼�����Ѵ�
# ���뤿��ˤϡ�
#
#     perl edict.perl --reverse edict >edict.sdic
#
# �� --reverse ���ץ������ɲä��Ƽ¹Ԥ��Ʋ������������������ޤ��ɤ�
# ���¼���������ޤ��󡣤������ɤ��Ʋ����������罸��Ǥ���


eval { binmode(STDOUT); };

if( $ARGV[0] eq '--reverse' ){
    shift;
    &reverse();
} else {
    &normal();
}
    
# �̾��SDIC�����μ������ؿ�
sub normal {
    $_ = <>;				# �ǽ��1�Ԥ����ɽ���Ǥ���
    s!^[^/]+/!!;
    s!/$!!;
    s!\s*$!\n!;				# ���ԥ����ɤ��Ѵ� [sdic:00428]
    print "# ",$_;
    while( <> ){
	s/\s+$//;			# �����ζ���ʸ������
	s!/\(P\)/$!!;			# �����Υޡ����������� [sdic:00430]
	s/&/&amp;/g;			# �᥿����饯�����ִ�����
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^([\200-\377]+) +//;		# ���Ф�����ڤ�Ф�
	$head = $1;
	$key  = $head;
	while( s/^\[([\200-\377]+)\] +// ){
	    $key .= ">$1";
	}
	s!^/!!;
	s!/$!!;
	$KEY{ sprintf("%s>%s",$head,$_) } .= ">$key";
    }
    for $str ( keys %KEY ){
	( $head,$content ) = split( />/,$str );
	$KEY{$str} =~ s/^>//;
	for( split(/>/,$KEY{$str}) ){
	    $key{$_}++;
	}
	push( @LINE,sprintf( "<K>%s</K>%s%s\n",
			     $head,
			     join( "", map( sprintf("<K>%s</K>",$_),
					    grep( $_ ne $head, sort keys %key ) ) ),
			     $content ) );
	undef %key;
    }
    undef %KEY;
    print sort @LINE;
}

# �ո�����SDIC�����μ������ؿ�
sub reverse {
    $_ = <>;				# �ǽ��1�Ԥ����ɽ���Ǥ���
    s!^[^/]+/!!;
    s!/$!!;
    s!\s*$!\n!;				# ���ԥ����ɤ��Ѵ� [sdic:00428]
    print "# ",$_;
    while( <> ){
	s/\s+$//;			# �����ζ���ʸ������
	s!/\(P\)/$!!;			# �����Υޡ����������� [sdic:00430]
	s/&/&amp;/g;			# �᥿����饯�����ִ�����
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^([\200-\377]+) +//;		# ���Ф�����ڤ�Ф�
	$content = $1;			# ���Ф��줬����ʸ�ˤʤ�
	while( s/^\[([\200-\377]+)\] +// ){ ; }	# ���겾̾��ΤƤ�
	s!^/!!;
	s!/$!!;
	for $head ( split( "/",$_ ) ){
	    $STR{$head} .= ">$content";
	}
    }
    for $head ( keys %STR ){
	for( split( />/,substr($STR{$head},1) ) ){
	    $content{$_}++;
	}
	for( keys %content ){
	    if( /^([\200-\377]{2})\1/ && $content{sprintf("%s��%s",$1,substr($_,4))} ){
		$content{$_}=0;
	    }
	}
	$str = $head;
	$str =~ tr/A-Z/a-z/;
	$str =~ s/\s+/ /;
	$key = $str;			# $key = ����ȱ���ʸ��/��ʸ������������Ԥʤä�ʸ����
	$str =~ s/^(\([^\)]+\) *)+//;
	$str =~ s/( *\([^\)]+\))+$//;
	$str =~ s/^~ //;
	$str =~ s/ ~$//;		# $str = (...) �� ~ ���������ʸ����
	$str = $key unless $str;
	if( $str eq $head ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<K>%s</K>%s\n",
				  $str, $head, $head,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} elsif( $key eq $head ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<K>%s</K><K>%s</K>%s\n",
				  $str, $head, $head, $str,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} elsif( $key eq $str ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<H>%s</H><K>%s</K>%s\n",
				  $str, $head, $head, $str,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} else {
	    push( @LINE, sprintf( "%s\x00%s\x00<<H>%s</H><K>%s</K><K>%s</K>%s\n",
				  $str, $head, $head, $str, $key,
				  join("/",grep($content{$_}>0,keys %content)) ));
	}
	undef %content;
    }
    undef %STR;
    for( sort @LINE ){
	@f = split( /</,$_,2 );
	print $f[1];
    }
}
