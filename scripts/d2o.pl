#! /usr/bin/perl -w

# ------------------------------------------------------------------
# Purpose   : d_file convert to o_file
# Created   : Xingyu Chen,  chenxingyu0201@163.com
# Copyright : GNSS Research Center, Wuhan University, 2016
# Usage     : d2o.pl file_path FLAG
# Example   : d2o.pl /home/cxy/pppar_20151110/335 FLAG
# ------------------------------------------------------------------

use strict;
use File::Copy;
use Cwd;

# Display help if no commad line arguments is given
if (scalar(@ARGV) < 1 or scalar(@ARGV) > 2){
    print "arguments error";
    exit;
}


# Assignment script command line arguments
my $dir = $ARGV[0];
my $flag ="LINKTABLE";

if(scalar(@ARGV)==2)
{
    $flag = $ARGV[1];
}

if (! -e "${dir}"){
    print "\n O file path does not exit!\n";
    exit;
}

my $crx2rnx = 'crx2rnx';
my $filecount =0;

my $count= parse_env($dir);
print $count."\n";

#--------------------------------------------------------------------
#            traverse folder
#--------------------------------------------------------------------
sub parse_env {
    my $path = shift;    #arguments
    my $subpath;
    my $handle;
    my $handle1;
    my $unzip = 'gzip';

    if (-d $path) {#当前路径是否为一个目录
        if (opendir($handle, $path)) {
            while ($subpath = readdir($handle)) {
                if (!($subpath =~ m/^\.$/) and !($subpath =~ m/^(\.\.)$/)) {
                    my $o_file = $path."/$subpath";
                    if (-d $o_file) {
                        parse_env($o_file);
                    }
                    else {
                        if(substr($o_file,(length($o_file)-2),(length($o_file))) eq '.Z' or substr($o_file,(length($o_file)-2),(length($o_file))) eq '.z'){
                            my $o_file1 = substr($o_file,0,(length($o_file)-2));
                            `$unzip -cd $o_file > $o_file1`;
                            unlink("$o_file");
                            if(substr($o_file1,(length($o_file1)-1),(length($o_file1))) eq 'd')
                            {
                                my $txt ="station_name.txt";
                                my $station = $path."/$txt";
                                system("$crx2rnx", "$o_file1");
                                unlink("$o_file1");
                                my $station_name = " ".substr($subpath,0,4)." S  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000";
                                open($handle1,">>$station");
                                print $handle1 $station_name."\n";
                                close($handle1);
                                ++$filecount;

                                print $o_file."\n";
                            }
                            elsif(substr($o_file1,(length($o_file1)-1),(length($o_file1))) eq 'o'){
                                my $txt ="station_name.txt";
                                my $station = $path."/$txt";
                                my $station_name = " ".substr($subpath,0,4)." S  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000";
                                open($handle1,">>$station");
                                print $handle1 $station_name."\n";
                                close($handle1);
                                ++$filecount;

                                print $o_file."\n";
                            }
                        }
                        elsif(substr($o_file,(length($o_file)-1),(length($o_file))) eq 'd'){
                            my $txt ="station_name.txt";
                            my $station = $path."/$txt";
                            system("$crx2rnx", "$o_file");
                            unlink("$o_file");
                            my $station_name = " ".substr($subpath,0,4)." S  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000";
                            open($handle1,">>$station");
                            print $handle1 $station_name."\n";
                            close($handle1);
                            ++$filecount;

                            print $o_file."\n";
                        }
                        elsif(substr($o_file,(length($o_file)-1),(length($o_file))) eq 'o'){
                            my $txt ="station_name.txt";
                            my $station = $path."/$txt";
                            my $station_name = " ".substr($subpath,0,4)." S  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000";
                            open($handle1,">>$station");
                            print $handle1 $station_name."\n";
                            close($handle1);
                            ++$filecount;

                            print $o_file."\n";
                        }
                    }
                }
            }
            closedir($handle);
        }
    }
    else
    {
        print "\n open folder error!\n";
        exit;
    }

    return $filecount;
}
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#                      Subroutine programs
#--------------------------------------------------------------------
#--------------------------------------------------------------------
sub MOD {
    my ($a1, $a2) = @_ ;
    return ($a1 - int($a1 / $a2) * $a2);
}
#--------------------------------------------------------------------
#           Transformation from YEAR and DOY to YMD
#--------------------------------------------------------------------
sub ydoy2md{
    my $iyear = shift;
    my $idoy = shift;
    my @days_in_month = (31,28,31,30,31,30,31,31,30,31,30,31);
    #my $days_in_month[2]=28;
    my $iday = "0";

    if(MOD($iyear,4) == 0 && (MOD($iyear,100) != 0 || MOD($iyear,400)==0)) {
        $days_in_month[1]=29;
    }
    my $id  = $idoy;
    my $imonth =0;
    foreach my $tmon (@days_in_month){
        $id = $id - $tmon;
        $imonth = $imonth + 1;
        if($id > 0) {next;}
        $iday = $id + $tmon;
        last;
    }
    return "$imonth $iday" ;
}
