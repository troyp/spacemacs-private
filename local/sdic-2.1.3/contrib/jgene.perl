#!/usr/bin/perl -w
#
# EJ to JE dictionary converter for gene.dic [1/25/1999]
#
# Copyright (C) 1998-1999 Satoru Takabayashi  All rights reserved.
#     This is free software with ABSOLUTELY NO WARRANTY.
#
# You can redistribute it and/or modify it under the terms of 
# the GNU General Public License version 2.
#
# gene.dic �򸵤��±Ѽ����������ޤ���
# gene.dic �ν񼰤˹�碌�Ƥ���ʤ�ν�����Ԥ��ޤ���
# ���ʤ�񼰤��ɤ줬����Τ�Ĵ������ΤϤʤ��ʤ����Ǥ�����
# Perl 5.003 �ʹߤ�ɬ�פǤ��� JPerl ��ư������ʬ����ޤ���
# ���Υե���������ܸ� EUC �Ǥʤ���Фʤ�ޤ���
#
# �Ȥ���: jgene.pl [--compat] < gene.dic > jgene.dic
#         --compat: �ߴ��⡼��
#   
#   Pentium 133MHz, 64MB �Υޥ������ 2ʬ������ޤ���
#
# ���Τ�������
#     * ���������Ѥ߲٤򤪤�	unload
#     *  ���Ȥθ򴹤�	in barter with ~
#     *  ��(����ʤ���)��и�������	put someone through ~
#   �Τ褦����Ƭ������ǻϤޤ��Τ򤽤Τޤޤˤ��Ƥ��롣
#


require 5.004;
use strict;
use IO::File;

my $CHOON     = "(?:[\xa1][\xbc])";  # ��
my $CHOON2    = "(?:[\xa1][\xc1])";  # ��
my $NAKAGURO  = "(?:[\xa1][\xa6])";  # ��
my $HIRAGANA  = "(?:[\xa4][\xa1-\xf3])";
my $KANJI     = "(?:[\xb0-\xfe][\xa1-\xfe]|\xa1\xb9)";
my $CHAR      = "(?:[\xa1-\xfe][\xa1-\xfe]|[^\xa1-\xfe])";

my $KAKASI = "kakasi";
my %je;

my $Compatible = 0;
$Compatible = 1, shift @ARGV if defined($ARGV[0]) && $ARGV[0] eq "--compat";

$| = 1;
print STDERR "reading input...\n";
eval { binmode(STDOUT); };
while(<>) {
    /(.*)\t(.*)/;
    my $term = $1;
    my $definition = $2;

    # �����ʤɤΥ���ޤ򥨥������פ��Ƥ���
    $definition =~ s/(\/.*)   /escape_commas($1)/e;
    $definition =~ s/(\(.*?\))/escape_commas($1)/ge;
    $definition =~ s/(\".*?\")/escape_commas($1)/ge;
    $definition =~ s/(\<.*?\>)/escape_commas($1)/ge;

    # ���ޡ������������ΰ�̣�˷Ҥ���
    #   cable lock	�磻���,���������padlock
    $definition =~ s/, *(��)/$1/g;

    my @meanings = split(/, */, $definition);
    for my $meaning (@meanings) {
	# �����ø����Ѵ���2�Ť˹Ԥ�
	$meaning = transform($meaning);
	$meaning = transform($meaning);

	# = �ޡ����ʤ�Ф��θ���ñ��ˤ��äĤ���
	# (����ܰʹߤι��ܤˤ��줬������ɤ��ʤ��������)
	#   business trend	=economic trend,�ʵ�ư��
	if ($meaning =~ /^=/) {
	    $meaning =~ s/\x0/,/g;
	    $term .= " $meaning";
	    next;
	}

	# ���������פ��Ƥ���������ޤ�����
	$meaning =~ s/\x0/,/g;

	# ��Ƭ�����ܸ�ʤ����Ͽ����
	$je{$meaning} .= $term . ", " if $meaning =~ /^[\xa1-\xfe]/;
    }
}

# �Ҥ餬�ʤ��ɤߤ� KAKASI �˺�äƤ�餦 and ����
# IPC::Open2 �Ϥ��ޤ����ʤΤǰ�ö�ե�����˽񤭽Ф�
if ($Compatible) {
    for my $tmp (sort keys (%je)) {
	$je{$tmp} =~ s/, *$//;
	print $tmp, "\t", $je{$tmp}, "\n";
    }
} else {
    print STDERR "working KAKASI...\n";
    my $fname = "tmp.$$";
    {
	my $fh = new IO::File;
	$fh->open("|$KAKASI -JH -KH > $fname") or die $!;
	for my $tmp (sort keys(%je)) {
	    if ($tmp =~ /^$CHAR*$KANJI/o) {
		print $fh $tmp, "\n";
	    } else {
		print $fh "\n";
	    }
	}
    }

    my @results = ();
    {
	my $fh = new IO::File;
	$fh->open("$fname") or die $!;
	@results = <$fh>;
    }

    print STDERR "outputting results to stdout...\n";
    my $i = 0;
    for my $tmp (sort keys (%je)) {
	$je{$tmp} =~ s/, *$//;
	$je{$tmp} =~ s/&/&amp;/g;
	$je{$tmp} =~ s/</&lt;/g;
	$je{$tmp} =~ s/>/&gt;/g;

	$results[$i++] =~ /^((?:$HIRAGANA|$CHOON|$CHOON2|$NAKAGURO)+)/o;
	my $yomi = $1;
	if ($yomi && $tmp =~ /^$CHAR*$KANJI/o) {
	    print "<K>$tmp</K>", "<K>$yomi</K>", $je{$tmp}, "\n";
	} else {
	    print "<K>$tmp</K>", $je{$tmp}, "\n";
	}
    }
    unlink $fname;
} 

# �����ޤ�

sub escape_commas ($) {
    my ($str) = @_;
    $str =~ s/,/\x0/g;
    $str;
}

sub transform($) {
    my ($meaning) = @_;

    # ��Ƭ�ζ����ΤƤ�
    $meaning =~ s/^ +//;
    
    # ���Ф��ο�����ʬ����
    # bottle  1.�ӥ�,��,2.�ӥ�������,�ӥ�������̩������,�ޤ���
    $meaning =~ s/^(\d+\. *)|(\(\d+\) *)//;

    # (the ��) ����
    # brink of bankruptcy (the ��)�˻������ͺ�
    $meaning =~ s/\(the .*?\)//;
	
    # ��Ƭ�� (a ��) ����
    # bottle of ~ (a ��)��ӥ��
    $meaning =~ s/^\(a .*?\)//;
	
    # ��Ƭ�� ? ����ػ��äƤ���
    # BIL  ? ���׷���ﶯ��
    $meaning =~ s/^\? (.*)/$1 ?/;
    
    # ��Ƭ�� ��...�� ����ػ��äƤ���
    #   British Columbia  ����̾�ۥ֥�ƥ��å��女���ӥ�;ά:BC
    # ��Ƭ�� ��...�� ����ػ��äƤ���
    #   beanery  1.���Ʋ��áհ¥쥹�ȥ��,�罰��Ʋ,2.���ưŹ�����α�־�
    # ��Ƭ�� <...> ����ػ��äƤ���
    #   benign  1.<�͡����ʤʤɤ�>���ڤ�,ͥ����,
    #           2.<���������ڤ�>�򹯤��ɤ�,���¤�
    # ��Ƭ�� (...) ����ػ��äƤ���
    #   captivate  (���ο���)�ᤨ��,̥�Ǥ���,�ͤ򤦤äȤꤵ����,����å��
    # ��Ƭ�� [...] ����ػ��äƤ���
    #   ciao	�԰ˡ�[��]<��>���㥪,����ˤ���,���褦�ʤ�

    $meaning =~ s/^((?:(?:��$CHAR*?��)|(?:��$CHAR*?��)|(?:<.*?>)|(?:\(.*?\))|(?:\[.*?\]) *)+)(.*)/$2 $1/;


    # ��Ƭ�� "..." ���� "" ����
    #   European Free Trade Association  "���ե�",������ͳ�ǰ�Ϣ��;ά:EFTA
    $meaning =~ s/"(.*?)"/$1/;
    
    $meaning;
}
