#! /usr/bin/perl -w

# ------------------------------------------------------------------
# Purpose   : GPS Standerd Precise Point Positioning(PPP)
#             and Ambiguity Resolution(PPP-AR)
# Created   : Xingyu chen,  chenxingyu@whu.edu.cn
# Copyright : GNSS Research Center, Wuhan University, 2018
# ------------------------------------------------------------------

# Display help if commad line arguments is error
if (scalar(@ARGV) < 1 or scalar(@ARGV) > 5){
    my $help = <<EOD;
 -----------------------------------------------------------------------
  Purpose  :      GPS Standerd Precise Point Positioning(PPP) and Ambiguity Resolution(AR)
  Created  :      Xingyu chen,  chenxingyu@whu.edu.cn
  Copyright:      GNSS Research Center, Wuhan University, 2018
  Usage    :      pridelab_pppar.pl ctrl_file StartYMD EndYMD AR/FR
  Example  :      pridelab_pppar.pl ses.ppp 20120101 20121230 AR/FR
                  FR   ----- Flot Resolution
                  AR   ----- Ambiguity Resolution
 -----------------------------------------------------------------------
EOD
    print $help;
    exit;
}

use strict;
use File::Copy;
use Cwd;

##########  Assignment Script Command Line Arguments  ################
# ctrl file
my $ctrl_file = $ARGV[0];
my $ctrl_file1 = "$ARGV[0]_ss";
# time arguments (YYYYMMDD)
my $start_year = substr($ARGV[1],0,4);
my $start_month = substr($ARGV[1],4,2);
my $start_day = substr($ARGV[1],6,2);
my $end_year = substr($ARGV[2],0,4);
my $end_month = substr($ARGV[2],4,2);
my $end_day = substr($ARGV[2],6,2);
# position mode
my $position_mode =$ARGV[3];
# Transformate from year,month and day to modified julian day
my $start_mjd = ymd2mjd($start_year,$start_month,$start_day);
my $end_mjd = ymd2mjd($end_year,$end_month,$end_day);
my $k = $start_mjd;
# set file path
my $tbdir = './';
my $tbdir1= '../';
if (! -e "${tbdir}$ctrl_file"){
    print "\n PANDA-PPP control file does not exit!\n";
    exit;
}
# Program names for GPS Precision Point Positioning
my $edtres  = 'redig';
my $lsq     = 'lsq';
my $pppar   = 'arsig';

my @jump   = (400,200,100,50,50,50,50);

########### process day by day #############
while($k <= $end_mjd){
    # Make Directory
    my $ydoy  = mjd2ydoy($k);                       # doy(day of year)
    my @ydoy  = split(' ',$ydoy);
    my $year4 = $ydoy[0];                           # 4 digital year
    my $year2 = substr($year4,2,2);                 # 2 digital year
    my $doy   = $ydoy[1];                           # day of year
    mkdir $year4;                                   # make working directory of year
    chdir("$year4");
    my $path  = mkdir $doy;                         # make working directory of doy
    print "--------------------+------------------+-----------------------\n";
    print "---------+-------------------+----------------------+----------\n";
    print "Control file directory: ${tbdir}${ctrl_file}\n";
    copy("${tbdir1}${ctrl_file}", "./$doy");
    chdir("$doy");                                  ########## Change into working directory of doy
    print "PROCESSING $ydoy DATA...\n";
    my $month = ydoy2md($year4,$doy);
    my @month = split(' ',$month);
    $month = $month[0];
    my $day   = $month[1];
    print "DATE is: $year4,$month,$day\n";
    # Set Processing Time
    my $hour    = 00;
    my $minute  = 00;
    my $second  = 00;
    my $session = 86360;
    my $tstart  = `get_ctrl $ctrl_file "Session time"`;
    my @tstart  = split(' ',$tstart);

    if($#tstart<6){
        print "\n Warnning: PANDA-PPP control file (Session time formal error)\n";
        $hour    = 00;
        $minute  = 00;
        $second  = 00;
        $session = 86360;
    }
    else {
        if ($tstart[3]<0 || $tstart[4]<0 || $tstart[5]<0 || $tstart[6]<0
            || $tstart[3]>23 || $tstart[4]>59 || $tstart[5]>59 || $tstart[6]>86360){
            print "\n Warnning: PANDA-PPP control file (Session time formal error)\n";
            $hour    = 00;
            $minute  = 00;
            $second  = 00;
            $session = 86360;
        }
        else {
            $hour    = $tstart[3];
            $minute  = $tstart[4];
            $second  = $tstart[5];
            $session = $tstart[6];
        }
    }
    update_ctrlfile($ctrl_file,$doy,$year4,$month,$day,$hour,$minute,$second,$session);

    #Compute GPS week
    my $weekdow  = ydoy2wkdow($year4,$month,$day);            # week and day of week
    my @weekdow  = split(' ',$weekdow);
    my $week     = $weekdow[0];                               # GPS week
    my $dow      = $weekdow[1];                               # day of week

    # Get PANDA tables,sp3 and rinex data path
    my $dirtbl = `get_ctrl $ctrl_file "Table directory"`;
    my $dirsp3 = `get_ctrl $ctrl_file "Sp3 directory"`;
    my $dirrnx = `get_ctrl $ctrl_file "Rinex directory"`;
    $dirtbl = substr($dirtbl,0,(length($dirtbl)-1));
    $dirsp3 = substr($dirsp3,0,(length($dirsp3)-1));
    $dirrnx = substr($dirrnx,0,(length($dirrnx)-1));
    if(! -d $dirtbl){
        print "\nPANDA tables directory does not exist!\n";
        print $dirtbl;
        exit;
    }
    if(! -d $dirsp3){
        print "\nPANDA sp3 directory does not exist!\n";
        exit;
    }
    if(! -d $dirrnx){
        print "\nGPS rinex directory does not exist!\n";
        exit;
    }
    ########## calculate prio coordinate #########
    if(! -e  "${dirrnx}/sit.xyz"){
        `rtk2xyz.sh $dirrnx`;
    }

    ########## update station lale ###############
    `mkdir problem`;
    my $station_name_path="${dirrnx}/station_name.txt";
    update_ctrlfile_table($ctrl_file,$station_name_path,"S");
    #update_ctrlfile_table($ctrl_file2,$station_name_path,"F");

    ##########  Data Preparation  ################
    # Copy PANDA tables to the working directory
    print "Link the panda tables\n";
    copy_panda_tables($ctrl_file);

    # make the initial orb: use code eph
    print "initial orbit file\n";
    sp3orb_staclk($k);

    ##########  Data Pre-processing  ################
    print "Pre-process the observation data file\n";
    turboedit($ctrl_file);

    ######### Data Cleaning ############
    my $repeat=0;
    open(my $flhdl1, "$ctrl_file")
        or die "Cannt open PANDA control file : $ctrl_file" ;
    my @sitename1 = ();
    while (<$flhdl1>){
        if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
            push @sitename1, substr($_,1,4);
        }
    }
    close($flhdl1);

    `cp $ctrl_file $ctrl_file1`;
    my $numsite = 0;
    # update_ctrlfile_removebias($ctrl_file1,"YES");
    while($numsite < scalar(@sitename1)){
        # delete the amb file
        if(-e "amb_${year4}${doy}"){ unlink("amb_${year4}${doy}"); }
        # delete the recclk file
        if(-e "rck_${year4}${doy}"){ unlink("rck_${year4}${doy}"); }
        # delete the ztd file
        if(-e "ztd_${year4}${doy}"){ unlink("ztd_${year4}${doy}"); }
        # delete the htg file
        if(-e "htg_${year4}${doy}"){ unlink("htg_${year4}${doy}"); }
        # delete the stt file
        if(-e "stt_${year4}${doy}"){ unlink("stt_${year4}${doy}"); }
        # delete the res file
        if(-e "res_${year4}${doy}"){ unlink("res_${year4}${doy}"); }
        # delete the pos file
        if(-e "pos_${year4}${doy}"){ unlink("pos_${year4}${doy}"); }
        # delete the con file
        if(-e "con_${year4}${doy}"){ unlink("con_${year4}${doy}"); }
        my $name1 = lc $sitename1[$numsite];
        my $pos  = "pos_${year4}${doy}";
        my $pos1 = "pos_${year4}${doy}_$name1";
        my $stt  = "stt_${year4}${doy}";
        my $stt1 = "stt_${year4}${doy}_$name1";
        my $res  = "res_${year4}${doy}";
        my $res1 = "res_${year4}${doy}_$name1";
        my $ztd = "ztd_${year4}${doy}";
        my $ztd1 = "ztd_${year4}${doy}_$name1";
        my $rck = "rck_${year4}${doy}";
        my $rck1 = "rck_${year4}${doy}_$name1";
        my $htg = "htg_${year4}${doy}";
        my $htg1 = "htg_${year4}${doy}_$name1";
        update_ctrlfile_single($ctrl_file1,$name1,"S");
        $repeat=0;
        while($repeat<scalar(@jump)){
            ########## Parameters estimation using LSQ estimator  #################
            # delete the rck file
            if(-e "rck_${year4}${doy}"){ unlink("rck_${year4}${doy}"); }
            # delete the ztd file
            if(-e "ztd_${year4}${doy}"){ unlink("ztd_${year4}${doy}"); }
            # delete the amb file
            if(-e "amb_${year4}${doy}"){ unlink("amb_${year4}${doy}"); }

            system("$lsq", "$ctrl_file1")==0 or die "###ERROR: $lsq $ctrl_file1";
            my $interval = `get_ctrl $ctrl_file1 "Interval"`;
            my $short = 600/$interval;
            # Edit SRIF output residual file for next SRIF
            system("$edtres","res_${year4}${doy}","-jmp","$jump[$repeat]", "-sht", "$short")==0
                or die "###ERROR: $edtres res_${year4}${doy} -jmp $jump[$repeat] -sht $short";
            # Update repeat point for parameter estimation
            $repeat=$repeat+1;
        }
        $numsite = $numsite + 1;
    }
    `rm $ctrl_file1`;

    ##########  Data Processing  ################
    if ($position_mode eq 'FR'){
        my $repeat=0;
        open(my $flhdl1, "$ctrl_file")
            or die "Cannt open PANDA control file : $ctrl_file" ;
        my @sitename1 = ();

        while (<$flhdl1>){
            if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
                push @sitename1, substr($_,1,4);
            }
        }
        close($flhdl1);

        `cp $ctrl_file $ctrl_file1`;
        my $numsite = 0;
        while($numsite < scalar(@sitename1)){
            # delete the amb file
            if(-e "amb_${year4}${doy}"){ unlink("amb_${year4}${doy}"); }
            # delete the recclk file
            if(-e "rck_${year4}${doy}"){ unlink("rck_${year4}${doy}"); }
            # delete the ztd file
            if(-e "ztd_${year4}${doy}"){ unlink("ztd_${year4}${doy}"); }
            # delete the ztd file
            if(-e "htg_${year4}${doy}"){ unlink("htg_${year4}${doy}"); }
            # delete the stt file
            if(-e "stt_${year4}${doy}"){ unlink("stt_${year4}${doy}"); }
            # delete the res file
            if(-e "res_${year4}${doy}"){ unlink("res_${year4}${doy}"); }
            # delete the pos file
            if(-e "pos_${year4}${doy}"){ unlink("pos_${year4}${doy}"); }
            # delete the pos file
            if(-e "con_${year4}${doy}"){ unlink("con_${year4}${doy}"); }

            my $name1 = lc $sitename1[$numsite];
            my $pos  = "pos_${year4}${doy}";
            my $pos1 = "pos_${year4}${doy}_$name1";
            my $stt  = "stt_${year4}${doy}";
            my $stt1 = "stt_${year4}${doy}_$name1";
            my $res  = "res_${year4}${doy}";
            my $res1 = "res_${year4}${doy}_$name1";
            my $ztd = "ztd_${year4}${doy}";
            my $ztd1 = "ztd_${year4}${doy}_$name1";
            my $rck = "rck_${year4}${doy}";
            my $rck1 = "rck_${year4}${doy}_$name1";
            my $htg = "htg_${year4}${doy}";
            my $htg1 = "htg_${year4}${doy}_$name1";

            update_ctrlfile_single($ctrl_file1,$name1,"S");    #if you need processing mode "K", then you should change the "S" to "K"
            system("$lsq", "$ctrl_file1")==0 or die "###ERROR: $lsq $ctrl_file1";
            rename("$pos","$pos1");
            rename("$stt","$stt1");
            rename("$res","$res1");
            rename("$ztd","$ztd1");
            rename("$rck","$rck1");
            rename("$htg","$htg1");
            $numsite = $numsite + 1;
        }
    }
    elsif($position_mode eq "AR"){
        my $repeat=0;
        open(my $flhdl1, "$ctrl_file")
            or die "Cannt open PANDA control file : $ctrl_file" ;
        my @sitename1 = ();
        while (<$flhdl1>){
            if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
                push @sitename1, substr($_,1,4);
            }
        }
        close($flhdl1);

        `cp $ctrl_file $ctrl_file1`;
        my $numsite = 0;
        while($numsite < scalar(@sitename1)){
            # delete the amb file
            if(-e "amb_${year4}${doy}"){ unlink("amb_${year4}${doy}"); }
            # delete the recclk file
            if(-e "rck_${year4}${doy}"){ unlink("rck_${year4}${doy}"); }
            # delete the ztd file
            if(-e "ztd_${year4}${doy}"){ unlink("ztd_${year4}${doy}"); }
            # delete the ztd file
            if(-e "htg_${year4}${doy}"){ unlink("htg_${year4}${doy}"); }
            # delete the stt file
            if(-e "stt_${year4}${doy}"){ unlink("stt_${year4}${doy}"); }
            # delete the res file
            if(-e "res_${year4}${doy}"){ unlink("res_${year4}${doy}"); }
            # delete the pos file
            if(-e "pos_${year4}${doy}"){ unlink("pos_${year4}${doy}"); }
            # delete the pos file
            if(-e "con_${year4}${doy}"){ unlink("con_${year4}${doy}"); }
            my $name1 = lc $sitename1[$numsite];
            my $pos  = "pos_${year4}${doy}";
            my $pos1 = "pos_${year4}${doy}_$name1";
            my $stt  = "stt_${year4}${doy}";
            my $stt1 = "stt_${year4}${doy}_$name1";
            my $res  = "res_${year4}${doy}";
            my $res1 = "res_${year4}${doy}_$name1";
            my $ztd = "ztd_${year4}${doy}";
            my $ztd1 = "ztd_${year4}${doy}_$name1";
            my $rck = "rck_${year4}${doy}";
            my $rck1 = "rck_${year4}${doy}_$name1";
            my $htg = "htg_${year4}${doy}";
            my $htg1 = "htg_${year4}${doy}_$name1";
            my $con = "con_${year4}${doy}";
            my $con1 = "con_${year4}${doy}_$name1";
            update_ctrlfile_single($ctrl_file1,$name1,"S");
            system("$lsq", "$ctrl_file1")==0 or die "###ERROR: $lsq $ctrl_file1";
            system("$pppar","$ctrl_file1")==0
                or die "###ERROR: $pppar $ctrl_file";
            system("$lsq", "$ctrl_file1")==0 or die "###ERROR: $lsq $ctrl_file1";
            rename("$pos","$pos1");
            rename("$stt","$stt1");
            rename("$res","$res1");
            rename("$ztd","$ztd1");
            rename("$rck","$rck1");
            rename("$htg","$htg1");
            $numsite = $numsite + 1;
        }
    }
    $k=$k+1;
    chdir("..");        ########## Return to the parent directory
    chdir("..");        ########## Return to the parent directory
}
print "********SUCESSFUL!!!********";

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
#                Transformation from YMD to MJD
#--------------------------------------------------------------------
sub ymd2mjd {
    my ($year,$month,$day) = @_;
    if( $month <= 2 ){
        $month = $month + 12 ;
        $year = $year - 1 ;
    }
    my $mjd  = $year*365.25 - MOD( $year*365.25 , 1 ) - 679006;
    $mjd += int(30.6001*($month+1)) + 2 - int($year/100) + int($year/400) + $day ;
    return $mjd ;
}
#--------------------------------------------------------------------
#           Transformation from MJD to YEAY and DOY
#--------------------------------------------------------------------
sub mjd2ydoy {
    my $mjd =shift;
    # Set default variables
    my @doys = (366,365,365,365);
    my $day52 = 34012;
    my $styr = 1952;
    my $year = $styr; my $day = ($mjd+1)-$day52; my $i = 0; while($day >
        $doys[$i]){ $day = $day-$doys[$i]; $year = $year+1; $i = ($year%4)+0; } my
    $doy = $day; if($doy < 10){ $doy = '00'.$doy; } if($doy > 9 && $doy < 100){
        $doy = '0'.$doy; } return "$year $doy";
}
#--------------------------------------------------------------------
#           Transformation from MYD to GPS WEEK
#--------------------------------------------------------------------
sub ydoy2wkdow {
    my $year = shift;
    my $month = shift;
    my $day = shift;
    # Set default variables: 1980.01.05 mjd = 44243
    my $mjd0 = 44243;
    my $mjd = ymd2mjd($year,$month,$day);
    my $difmjd = $mjd-$mjd0-1;
    my $week = int($difmjd/7);
    my $dow = $difmjd%7;
    return "$week $dow";
}
#--------------------------------------------------------------------
#           Transformation from YEAR and DOY to YMD
#--------------------------------------------------------------------
sub ydoy2md{
    my $iyear = shift;
    my $idoy = shift;
    my @days_in_month = (31,28,31,30,31,30,31,31,30,31,30,31);
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
#--------------------------------------------------------------------
#           Update the control file for sigle station
#--------------------------------------------------------------------
sub update_ctrlfile_removebias {
    my ($ctrlfile,$site_type) = @_;
    my $th=0;
    open(my $oldfile, "$ctrlfile")
        or die "Couldn't open $ctrlfile for reading: $!";

    open(my $newfile, ">${ctrlfile}.back")
        or die "Couldn't open ${ctrlfile}.back for writing: $!";

    while(<$oldfile>){
        if($_=~ /^Remove bias/){
            print $newfile "Remove bias      =$site_type\n";
        }
        else{
            print $newfile $_;
        }
    }
    close($oldfile);
    close($newfile);
    rename("${ctrlfile}.back","$ctrlfile");
}
#--------------------------------------------------------------------
#           Update the control file with station table
#--------------------------------------------------------------------
sub update_ctrlfile_table {
    my ($ctrlfile,$path,$site_type) = @_;
    my $th=0;

    open(my $flhdl1, "${path}")
        or die "Cannt open PANDA control file : $ctrl_file" ;
    my @sitename11 = ();
    while (<$flhdl1>){
        if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
            if(substr($_,1,4)=~"marn"){
                next;
            }
            if(!(substr($_,1,4)~~@sitename11)){
                push @sitename11, substr($_,1,4);
            }
        }
    }
    close($flhdl1);
    open(my $oldfile, "$ctrlfile")
        or die "Couldn't open $ctrlfile for reading: $!";
    open(my $newfile, ">${ctrlfile}.back")
        or die "Couldn't open ${ctrlfile}.back for writing: $!";
    while(<$oldfile>){
        my $key = "+Station used ";
        if($_=~ /^\*NAME/){
            print $newfile $_;
            my $numsite11 = 0;
            while($numsite11 < scalar(@sitename11)){
                my $station_name = lc $sitename11[$numsite11];
                print $newfile " $station_name $site_type  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000\n";
                $numsite11 = $numsite11 + 1;
            }
            print $newfile "-Station used\n";
            goto OVER;
        }
        else{
            print $newfile $_;
        }
    }
    OVER:
    close($oldfile);
    close($newfile);
    rename("${ctrlfile}.back","$ctrlfile");
}
#--------------------------------------------------------------------
#           Update the control file for sigle station
#--------------------------------------------------------------------
sub update_ctrlfile_single {
    my ($ctrlfile,$station_name,$site_type) = @_;
    my $th=0;
    open(my $oldfile, "$ctrlfile")
        or die "Couldn't open $ctrlfile for reading: $!";

    open(my $newfile, ">${ctrlfile}.back")
        or die "Couldn't open ${ctrlfile}.back for writing: $!";

    while(<$oldfile>){
        my $key = "+Station used ";
        if($_=~ /^*NAME/){
            print $newfile $_;
            print $newfile " $station_name $site_type  GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000\n";
            print $newfile "-Station used\n";
            goto OVER;
        }
        else{
            print $newfile $_;
        }
    }
    OVER:
    close($oldfile);
    close($newfile);
    rename("${ctrlfile}.back","$ctrlfile");
}
#--------------------------------------------------------------------
#           Update the control file for useful station
#--------------------------------------------------------------------
sub update_ctrlfile_delsta {
    my ($ctrlfile,$station_name) = @_;
    my $th=0;
    open(my $oldfile, "$ctrlfile")
        or die "Couldn't open $ctrlfile for reading: $!";

    open(my $newfile, ">${ctrlfile}.back")
        or die "Couldn't open ${ctrlfile}.back for writing: $!";

    while(<$oldfile>){
        if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
            if($_=~ $station_name){}
            else{
                print $newfile $_;
            }
        }
        else{
            print $newfile $_;
        }
    }
    close($oldfile);
    close($newfile);
    rename("${ctrlfile}.back","$ctrlfile");
}

#--------------------------------------------------------------------
#           Update the control file for Session configure
#--------------------------------------------------------------------
sub update_ctrlfile {
    my ($ctrlfile,$doy,$year,$month,$day,$hour,$minute,$sec,$session) = @_;
    my $th=0;
    open(my $oldfile, "$ctrlfile")
        or die "Couldn't open $ctrlfile for reading: $!";

    open(my $newfile, ">${ctrlfile}.back")
        or die "Couldn't open ${ctrlfile}.back for writing: $!";

    while(<$oldfile>){
        if($_=~ /^Session time/){
            $th=$hour+0;
            if($hour<10){$hour="0$th";}
            print $newfile "Session time     = $year $month $day $hour $minute $sec $session\n";
        }
        elsif($_=~ /^Broadcast directory/){
            $th=$doy+0;
            if($doy<10){$doy="00$th";}
            elsif($doy<100 and $doy>=10){$doy="0$th";}
            my $dirbrdc = `get_ctrl $ctrl_file "Broadcast directory"`;
            $dirbrdc = substr($dirbrdc,0,(length($dirbrdc)-1));
            substr($dirbrdc,-8) = $year."/".$doy."\n";
            print $newfile "Broadcast directory = $dirbrdc";
        }
        elsif($_=~ /^Rinex directory/){
            $th=$doy+0;
            if($doy<10){$doy="00$th";}
            elsif($doy<100 and $doy>=10){$doy="0$th";}
            my $dirrnx = `get_ctrl $ctrl_file "Rinex directory"`;
            $dirrnx = substr($dirrnx,0,(length($dirrnx)-1));
            substr($dirrnx,-8) = $year."/".$doy."\n";
            print $newfile "Rinex directory     = $dirrnx";
        }
        else{
            print $newfile $_;
        }
    }
    close($oldfile);
    close($newfile);
    rename("${ctrlfile}.back","$ctrlfile");
}

#--------------------------------------------------------------------
#         Copy table files into the current working directory
#--------------------------------------------------------------------
sub copy_panda_tables {
    my $ctrl_file = shift;
    my @tables = ( "jpleph_de405" , "leap.sec" , "oceanload" ,
        "file_name" , "abs_igs.atx" , "sit.xyz" );
    my $dirtbl = '';
    $dirtbl = `get_ctrl $ctrl_file "Table directory"`;
    $dirtbl = substr($dirtbl,0,length($dirtbl)-1);
    if(-d $dirtbl){
        foreach my $table (@tables){
            if(! -e "${dirtbl}/${table}"){
                print "***WARNING: ${dirtbl}/${table} is not exist!\n";
                next;
            }
            copy("${dirtbl}/${table}","./");
        }
    }
    else{
        print "\nThe PANDA table derectory does not exit\n";
        print " Derectory : $dirtbl";
    }
    my $dirrnx1 = `get_ctrl $ctrl_file "Rinex directory"`;
    $dirrnx1 = substr($dirrnx1,0,(length($dirrnx1)-1));
    if(-d $dirrnx1){
        copy("${dirrnx1}/sit.xyz","./");
    }
}

#--------------------------------------------------------------------
#           Pre-process the observation data file
#--------------------------------------------------------------------
sub turboedit {
    my $ctrl_file = shift;
    if (! -e $ctrl_file){
        print "\nPANDA control file does not exit!\n";
        exit;
    }

    # Set default programs for rinex data pre-processing
    my $crx2rnx   = 'crx2rnx';
    my $unzip     = 'gzip';
    my $wget      = 'wget';
    my $turboedit = 'tedit';

    # Get session start time and length of session
    my $tstart = `get_ctrl $ctrl_file "Session time"`;

    # Convert and calculate date and time using @tstart
    my @tstart  = split(' ',$tstart);
    my $year4   = $tstart[0];                           # 4 digital year
    my $year2   = substr($year4,2,2);                   # 2 digital year
    my $month   = $tstart[1];                           # month
    my $day     = $tstart[2];                           # day
    my $hour    = $tstart[3];
    my $minute  = $tstart[4];
    my $mjd     = ymd2mjd($year4,$month,$day);          # mjd
    my $ydoy    = mjd2ydoy($mjd);                       # year and doyofyear
    my @ydoy    = split(' ',$ydoy);
    my $doy     = $ydoy[1];                             # dayofyear
    my $weekdow = ydoy2wkdow($year4,$month,$day);       # week and dayofweek
    my @weekdow = split(' ',$weekdow);
    my $week    = $weekdow[0];                          # week
    my $dow     = $weekdow[1];                          # dayofweek
    my $sesslen = $tstart[6];                           # session length

    # Get brodcast name from rinex data directory
    my $dirrnx  = `get_ctrl $ctrl_file "Rinex directory"`;
    my $dirbrdc = `get_ctrl $ctrl_file "Broadcast directory"`;
    $dirrnx     = substr($dirrnx,0,(length($dirrnx)-1));
    $dirbrdc    = substr($dirbrdc,0,(length($dirbrdc)-1));

    if(! -d $dirrnx){
        print "\nGPS rinex directory does not exit!\n";
        exit;
    }
    if(! -d $dirbrdc){
        print "\nGPS brodcast directory does not exit!\n";
        exit;
    }

    #delete the log file
    unlink(glob("rhd_*"));     # delete

    my $brdc = "${doy}0.${year2}n";
    if(-e "$dirbrdc/brdc${brdc}"){
        $brdc = "$dirbrdc/brdc${brdc}";
    }
    elsif(-e "$dirbrdc/brdc${brdc}.Z"){
        `$unzip -cd $dirbrdc/brdc${brdc}.Z > $dirbrdc/brdc${brdc}`;
        $brdc = "brdc${brdc}";
    }
    else{
        system("$wget", "-nv", "-nc","-t 3","--connect-timeout=10","--read-timeout=60",
            "ftp://igs.gnsswhu.cn/pub/gps/data/daily/${year4}/$doy/${year2}n/brdc${doy}0.${year2}n.Z", "-P", "$dirbrdc") ==0
            or die "###ERROR: brdc file not find, ftp://igs.gnsswhu.cn/pub/gps/data/daily/${year4}/$doy/${year2}n/brdc${doy}0.${year2}n.Z\n";
        $brdc = "$dirbrdc/brdc${doy}0.${year2}n.Z";
        `$unzip -cd $brdc > "$dirbrdc/brdc${doy}0.${year2}n"`;
        $brdc = "$dirbrdc/brdc${doy}0.${year2}n";
    }

    # Get GPS rinex observable data interval
    my $interval = `get_ctrl $ctrl_file "Interval"`;
    $interval = substr($interval,0,length($interval)-1);

    open(my $flhdl, "$ctrl_file")
        or die "Cannt open PANDA control file : $ctrl_file" ;
    my @sitename = ();
    my @sitetype = ();
    my @siteelev = ();
    my @x        = ();
    my @y        = ();
    my @z        = ();

    while (<$flhdl>){
        if($_ =~ m/^\s(\w{4})\s\w{1}\s{2}\w{3}/){
            push @sitename, substr($_,1,4);
            push @sitetype, substr($_,6,1);
            push @siteelev, substr($_,18,2);
        }
    }
    close($flhdl);

    my $numsite = 0;
    while($numsite < scalar(@sitename)){
        my $sitename = lc $sitename[$numsite];
        my $logflnam = "rhd_${year4}${doy}_$sitename";
        my $rnxflnam = "${dirrnx}/$sitename${doy}0.${year2}";
        my $rinexdata = '';
        my $x= '';
        my $y= '';
        my $z= '';

        open($flhdl, "sit.xyz")
            or die "Cannt open PANDA site file : sit.xyz" ;

        seek($flhdl,0,0);
        my $line = "";   #`get_ctrl $ctrl_file $site"_POS"`;
        my $site_up =uc($sitename);
        while (<$flhdl>){
            if($_ =~ /$sitename/ or $_ =~ /$site_up/){
                my @line = split(' ', $_);
                $x=$line[1];
                $y=$line[2];
                $z=$line[3];
                $_= ' ';
                goto OUT;
            }
        }

        print "***WARNING*** station $sitename no position!\n";
        update_ctrlfile_delsta($ctrl_file,$sitename);
        goto ADD;

        OUT:
        close($flhdl);

        if(-e "${rnxflnam}o"){
            $rinexdata = "${rnxflnam}o";
        }
        elsif(-e "${rnxflnam}o.Z") {
            `$unzip -cd "${dirrnx}\\${rnxflnam}o.Z" > "${rnxflnam}o"`;
            $rinexdata = "${rnxflnam}o";
        }
        elsif(-e "${rnxflnam}d"){
            `$crx2rnx "${rnxflnam}d"`;
            `move "$sitename${doy}0.${year2}o" "${dirrnx}"`;
            $rinexdata = "${rnxflnam}o";
            unlink("${rnxflnam}d");
        }
        elsif(-e "${rnxflnam}d.Z"){
            copy("${rnxflnam}d.Z","${dirrnx}\\${sitename}.Z");
            `$unzip -cd "${dirrnx}\\${sitename}.Z" > "${rnxflnam}d"`;
            unlink("${dirrnx}\\${sitename}.Z");
            $rnxflnam =~ s/\//\\/g;
            $rinexdata = "${rnxflnam}o";
            system("$crx2rnx","${rnxflnam}d","-o ","$rinexdata");
            $rinexdata = "${rnxflnam}o";
            unlink("${rnxflnam}d");
        }
        else{
            print "***WARNING*** station $sitename rinex data does not exit!\n";
            update_ctrlfile_delsta($ctrl_file,$sitename);
            goto ADD;
        }

        my $biasfile = 'p1c1bias.hist';
        if(! -e $biasfile){
            print "\nBias file $biasfile deos not exit!\n";
            exit;
        }
        if(! -e $logflnam){
            if($sitetype[$numsite] eq "S" or $sitetype[$numsite] eq "F"){
                #print "$turboedit $rinexdata -int $interval -rnxn $brdc -xyz $x $y $z -short 1200 -lc_check only -rhd $logflnam -pc_check 300 -elev $siteelev[$numsite] -len 86400 -time $year4 $month $day $hour $minute 00\n" ;
                `$turboedit $rinexdata -int $interval -rnxn $brdc -xyz $x $y $z -len 86400 -short 1200 -lc_check only -rhd $logflnam -pc_check 300 -elev $siteelev[$numsite] -time $year4 $month $day $hour $minute 00`;
            }
            elsif($sitetype[$numsite] eq "K"){
                #print "$turboedit $rinexdata -int $interval -rnxn $brdc -xyz $x $y $z -short 1 -lc_check no -rhd $logflnam -len $sesslen -time $year4 $month $day $hour $minute 00\n";
                `$turboedit $rinexdata -int $interval -rnxn $brdc -xyz $x $y $z -short 1 -lc_check no -rhd $logflnam -len 86400 -time $year4 $month $day $hour $minute 00`;
            }
            else{
                print "\nType of station $sitename[$numsite] does not right!\n";
                exit;
            }
        }

        ######################################################################################
        if(! -e $logflnam){
            print "***WARNING***: rhd file $logflnam deos not exit!\n";
            update_ctrlfile_delsta($ctrl_file,$sitename);
        }
        else{
            open($flhdl, $logflnam);
            seek($flhdl,0,0);
            my $line = "";
            my $numsite1 =$numsite+1;
            my @line = "";
            my $epo_ava=0;
            my $rem=0;
            while (<$flhdl>)
            {
                if($_ =~ "EPO AVA/REM/NEW"){
                    @line = split(' ', $_);
                    $epo_ava=sprintf "%.2f",$line[0];
                    $rem=sprintf "%.2f",$line[1];
                    if($epo_ava == 0 || $rem == 0 )
                    {
                        print "###WARNING: no rhd data $sitename \n";
                        update_ctrlfile_delsta($ctrl_file,$sitename);
                        `rm $logflnam`;
                        goto ADD;
                    }
                }
                if($_ =~ "EFF EPO/SUM/NEW")
                {
                    my @line = split(' ', $_);
                    my $eposum=sprintf "%.2f",$line[0];
                    my $nepo=sprintf "%.2f",$line[1];
                    if($eposum == 0 || $nepo == 0 )
                    {
                        print "###WARNING: no rhd data $sitename \n";
                        update_ctrlfile_delsta($ctrl_file,$sitename);
                        `rm $logflnam`;
                        goto ADD;
                    }
                    my $percent  = sprintf "%.2f", $epo_ava/($epo_ava+$rem)*100;
                    my $percent1 = sprintf "%.2f", $eposum/($nepo)*100;
                    if($percent <= 80)
                    {
                        print "###WARNING(get_lsq_args): bad observation quality $sitename \n";
                        update_ctrlfile_delsta($ctrl_file,$sitename);
                        `mv $logflnam ./problem/`;
                    }
                    if($percent1 <= 50)
                    {
                        print "###WARNING(get_lsq_args): short observation data $sitename \n";
                        update_ctrlfile_delsta($ctrl_file,$sitename);
                        `mv $logflnam ./problem/`;
                    }
                    print "STA: $numsite1 $sitename $rinexdata $percent% $percent1%\n";
                }
            }
            close($flhdl);
        }
        #################################################################################
        ADD:
        $numsite = $numsite + 1;
    }
    # Come to the end
    print "\nFinish for batch pre-processing rinex data by using turboedit program\n";
}

#--------------------------------------------------------------------
#             Produce igs-file and clock bias file
#--------------------------------------------------------------------
sub sp3orb_staclk{
    my $mjd   = shift;
    # Program names for GPS sp3 orbit
    my $sp3orb    = 'sp3orb';
    my $wget      = 'wget';
    my $mergesp3  = 'mergesp3';
    my $mergeerp  = 'mergeerp';
    my $unzip     = 'gzip';
    # compute the date of mjd day
    my $ydoy  = mjd2ydoy($mjd);
    my @ydoy  = split(' ',$ydoy);
    my $year4 = $ydoy[0];                                # 4 digital year
    my $year2 = substr($year4,2,2);                      # 2 digital year
    my $doy   = $ydoy[1];                                # day of year
    my $month = ydoy2md($year4,$doy);
    my @month = split(' ',$month);
    $month = $month[0];
    my $day   = $month[1];
    # Compute the date of mjd-1 day for mergeing sp3
    my $ydoy2  = mjd2ydoy($mjd- 1);                      # year and doy of last day
    my @ydoy2  = split(' ',$ydoy2);
    my $year42 = $ydoy2[0];                              # year of last day
    my $doy2   = $ydoy2[1];                              # doy of last day
    my $month2 = ydoy2md($year42,$doy2);
    my @month2 = split(' ',$month2);
    $month2 = $month2[0];
    my $day2   = $month2[1];

    # Compute the date of mjd+1 day for mergeing sp3
    my $ydoy1  = mjd2ydoy($mjd+1);                       # year and doy of next day
    my @ydoy1  = split(' ',$ydoy1);
    my $year41 = $ydoy1[0];                              # year of next day
    my $doy1   = $ydoy1[1];                              # doy of next day
    my $month1 = ydoy2md($year41,$doy1);
    my @month1 = split(' ',$month1);
    $month1 = $month1[0];
    my $day1   = $month1[1];
    # Compute the date of mjd-7 day for mergeing sp3
    my $ydoy22  = mjd2ydoy($mjd-7);                      # year and doy of last day
    my @ydoy22  = split(' ',$ydoy22);
    my $year422 = $ydoy22[0];                              # year of last day
    my $doy22   = $ydoy22[1];                              # doy of last day
    my $month22 = ydoy2md($year422,$doy22);
    my @month22 = split(' ',$month22);
    $month22 = $month22[0];
    my $day22   = $month22[1];
    # Compute the date of mjd+7 day for mergeing sp3
    my $ydoy11  = mjd2ydoy($mjd+7);                       # year and doy of next day
    my @ydoy11  = split(' ',$ydoy11);
    my $year411 = $ydoy11[0];                              # year of next day
    my $doy11   = $ydoy11[1];                              # doy of next day
    my $month11 = ydoy2md($year411,$doy11);
    my @month11 = split(' ',$month11);
    $month11 = $month11[0];
    my $day11   = $month11[1];

    # Compute GPS week
    my $weekdow  = ydoy2wkdow($year4,$month,$day);            # week and dayofweek
    my @weekdow  = split(' ',$weekdow);
    my $week     = $weekdow[0];                               # GPS week
    my $dow      = $weekdow[1];                               # dayofweek

    my $weekdow2 = ydoy2wkdow($year42,$month2,$day2);         # week and dayofweek of last day
    my @weekdow2 = split(' ',$weekdow2);
    my $week2    = $weekdow2[0];                              # GPS week of last day
    my $dow2     = $weekdow2[1];                              # dayofweek of last day

    my $weekdow1 = ydoy2wkdow($year41,$month1,$day1);         # week and dayofweek of next day
    my @weekdow1 = split(' ',$weekdow1);
    my $week1    = $weekdow1[0];                              # GPS week of next day
    my $dow1     = $weekdow1[1];                              # dayofweek of next day

    my $weekdow22 = ydoy2wkdow($year422,$month22,$day22);         # week and dayofweek of last day
    my @weekdow22 = split(' ',$weekdow22);
    my $week22    = $weekdow22[0];                              # GPS week of last day
    my $dow22     = $weekdow22[1];                              # dayofweek of last day

    my $weekdow11 = ydoy2wkdow($year411,$month11,$day11);         # week and dayofweek of next day
    my @weekdow11 = split(' ',$weekdow11);
    my $week11    = $weekdow11[0];                              # GPS week of next day
    my $dow11     = $weekdow11[1];                              # dayofweek of next day

    # Precision orbit sp3 and satellite clock bias
    my $GpsRefer  = "igs${week}${dow}.sp3";
    my $erprefer  = "igs${week}${dow}.erp";
    my $erprefer7   = "igs${week}7.erp";
    my $erprefer71  = "igs${week11}7.erp";
    my $erprefer72  = "igs${week22}7.erp";
    if($month<10){$month="0$month";}
    my $dcbrefer  = "P1C1${year2}${month}_RINEX.DCB";
    my $dcbrefer1 = "P2C2${year2}${month}_RINEX.DCB";
    my $erprefer1 = "igs${week11}${dow1}.erp";
    my $erprefer2 = "igs${week22}${dow2}.erp";
    my $GpsRefer2 = "igs${week2}${dow2}.sp3";
    my $GpsRefer1 = "igs${week1}${dow1}.sp3";
    my $GpsSatclk = "igs${week}${dow}.clk";

    # Get PANDA sp3 file path
    my $dirsp3 = `get_ctrl $ctrl_file "Sp3 directory"`;
    $dirsp3 = substr($dirsp3,0,(length($dirsp3)-1));

    # Produce initial orbit file and ics-file
    if(! -e "$GpsRefer"){
        copy("${dirsp3}/cod${week}${dow}.eph","./");
        rename("cod${week}${dow}.eph","$GpsRefer");
        if(! -e "$GpsRefer"){
            system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60",
                "ftp://igs.gnsswhu.cn/pub/gps/products/${week}/cod${week}${dow}.eph.Z");
            if(! -e "cod${week}${dow}.eph.Z"){
                system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60","ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}${dow}.EPH.Z") == 0
                    or die "###ERROR: Satelite eph file not find, $GpsRefer!\n ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}${dow}.EPH.Z\n";
                rename("COD${week}${dow}.EPH.Z","cod${week}${dow}.eph.Z");
            }
            my $eph = "cod${week}${dow}.eph.Z";
            `$unzip -cd $eph > "cod${week}${dow}.eph"`;
            `rm "cod${week}${dow}.eph.Z"`;
            copy("cod${week}${dow}.eph","${dirsp3}/");
            rename("cod${week}${dow}.eph","$GpsRefer");
        }
    }

    if(! -e "$erprefer"){                                    ##### the day #####
        copy("${dirsp3}/$erprefer","./");
        if(! -e "$erprefer"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}${dow}.ERP.Z`;
            if(! -e "COD${week}${dow}.ERP.Z"){
                if(${dow}>0 & ${dow}<6){
                    if(! -e "$erprefer7"){
                        copy("${dirsp3}/$erprefer7","./");
                        if(! -e "$erprefer7"){
                            system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60","ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}7.ERP.Z") == 0 or die "###ERROR: Satellite erp file not find, $erprefer7!\n ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}7.ERP.Z\n";
                            my $erp = "COD${week}7.ERP.Z";
                            `$unzip -cd $erp > "COD${week}7.ERP"`;
                            `rm "COD${week}7.ERP.Z"`;
                            `mv 'COD${week}7.ERP' $erprefer`;
                        }
                        else{
                            `mv "$erprefer7" $erprefer`;
                        }
                    }
                    else{
                        `mv "$erprefer7" $erprefer`;
                    }
                }
                else{
                    if(! -e "$erprefer7"){
                        copy("${dirsp3}/$erprefer7","./");
                        if(! -e "$erprefer7"){
                            system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60","ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}7.ERP.Z") == 0 or die "###ERROR: Satellite erp file not find, $erprefer7!\n ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}7.ERP.Z\n";
                            my $erp = "COD${week}7.ERP.Z";
                            `$unzip -cd $erp > "COD${week}7.ERP"`;
                            `rm "COD${week}7.ERP.Z"`;
                            `mv 'COD${week}7.ERP' $erprefer7`;
                        }
                    }
                    if(! -e "$erprefer71"){    ## next week erp
                        copy("${dirsp3}/$erprefer71","./");
                        if(! -e "$erprefer71"){
                            system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60","ftp://ftp.aiub.unibe.ch/CODE/${year411}/COD${week11}7.ERP.Z") == 0 or die "###ERROR: Satellite erp file not find, $erprefer71!\n ftp://ftp.aiub.unibe.ch/CODE/${year411}/COD${week11}7.ERP.Z\n";
                            my $erp = "COD${week11}7.ERP.Z";
                            `$unzip -cd $erp > "COD${week11}7.ERP"`;
                            `rm "COD${week11}7.ERP.Z"`;
                            `mv 'COD${week11}7.ERP' $erprefer71`;
                        }
                    }
                    if(! -e "$erprefer72"){
                        copy("${dirsp3}/$erprefer72","./");
                        if(! -e "$erprefer72"){
                            system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60", "ftp://ftp.aiub.unibe.ch/CODE/${year422}/COD${week22}7.ERP.Z") == 0 or die "###ERROR: Satellite erp file not find, $erprefer72!\n ftp://ftp.aiub.unibe.ch/CODE/${year422}/COD${week22}7.ERP.Z\n";
                            my $erp = "COD${week22}7.ERP.Z";
                            `$unzip -cd $erp > "COD${week22}7.ERP"`;
                            `rm "COD${week22}7.ERP.Z"`;
                            `mv 'COD${week22}7.ERP' $erprefer72`;
                        }
                    }
                    #######  mergeerp to meger erp file  ######
                    system("$mergeerp","$erprefer72","$erprefer7","$erprefer71","mer${erprefer}")==0 or die "###ERROR:$mergeerp $erprefer72 $erprefer7 $erprefer71 mer${erprefer}\n";
                    rename("mer${erprefer}","$erprefer");
                    unlink("$GpsRefer2");
                    unlink("$GpsRefer1");
                }
            }
            else{
                my $erp = "COD${week}${dow}.ERP.Z";
                `$unzip -cd $erp > "COD${week}${dow}.ERP"`;
                `rm "COD${week}${dow}.ERP.Z"`;
                `mv 'COD${week}${dow}.ERP' $erprefer`;
            }
            copy("$erprefer","${dirsp3}/");
        }
    }
    rename("$erprefer","igserp");

    if(! -e "P1C1.dcb"){                                    ##### the Month P1C1#####
        copy("${dirsp3}/$dcbrefer","./");
        if(! -e "$dcbrefer"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year4}/P1C1${year2}${month}_RINEX.DCB.Z`;
            if(! -e "P1C1${year2}${month}_RINEX.DCB.Z"){
                die "###ERROR: Satelite dcb P1C1 file not find, ftp://ftp.aiub.unibe.ch/CODE/${year4}/P1C1${year2}${month}_RINEX.DCB.Z\n";
            }
            my $dcb = "$dcbrefer.Z";
            `$unzip -cd $dcb > "$dcbrefer"`;
            `rm "$dcbrefer.Z"`;
            copy("$dcbrefer","${dirsp3}/");
        }
        `mv $dcbrefer "P1C1.dcb"`;
    }

    if(! -e "P2C2.dcb"){                                    ##### the Month P2C2#####
        copy("${dirsp3}/$dcbrefer1","./");
        if(! -e "$dcbrefer1"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year4}/P2C2${year2}${month}_RINEX.DCB.Z`;
            my $dcb = "$dcbrefer1.Z";
            `$unzip -cd $dcb > "$dcbrefer1"`;
            `rm "$dcbrefer1.Z"`;
            copy("$dcbrefer1","${dirsp3}/");
        }
        rename("$dcbrefer1","P2C2.dcb");
    }

    #  if(! -e "$erprefer2"){                                   ##### last day #####
    #       copy("${dirsp3}/$erprefer2","./");
    #       if(! -e "$erprefer2"){
    #       system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60",
    #                    "ftp://igs.gnsswhu.cn/pub/gps/products/${week22}/$erprefer2.Z") == 0
    #                 or die "###ERROR: Satelite erp file not find, $erprefer!\n ftp://igs.gnsswhu.cn/pub/gps/products/${week22}/$erprefer2.Z\n";
    #           my $erp2 = "$erprefer2.Z";
    #           `$unzip -cd $erp2 > "$erprefer2"`;
    #           `rm "$erprefer2.Z"`;
    #           copy("$erprefer2","${dirsp3}/");
    #       }
    #  }
    #
    #  if(! -e "$erprefer1"){                                   ##### next day #####
    #       copy("${dirsp3}/$erprefer1","./");
    #       rename("$erprefer1","$erprefer1");
    #       if(! -e "$erprefer1"){
    #           system("$wget", "-nv","-nc","-t 3","--connect-timeout=10","--read-timeout=60",
    #                       "ftp://igs.gnsswhu.cn/pub/gps/products/${week11}/$erprefer1.Z") == 0
    #               or die "###ERROR: Satelite Erp file not find, $erprefer1!\n ftp://garner.ucsd.edu/pub/products/${week11}/$erprefer1\n";
    #           my $erp1 = "$erprefer1.Z";
    #           `$unzip -cd $erp1 > "$erprefer1"`;
    #           `rm "$erprefer1.Z"`;
    #           copy("$erprefer1","${dirsp3}/");
    #       }
    #  }
    #########  mergeerp to meger erp file  ######
    #  system("$mergeerp","$erprefer2","$erprefer","$erprefer1","mer${erprefer}")==0
    #        or die "###ERROR:$mergeerp $erprefer2 $erprefer $erprefer1 mer${erprefer}\n";
    #  rename("mer${erprefer}","$erprefer");
    #  unlink("$GpsRefer2");
    #  unlink("$GpsRefer1");
    rename("$erprefer","igserp");

    if(! -e "$GpsRefer2"){
        copy("${dirsp3}/cod${week2}${dow2}.eph","./");
        rename("cod${week2}${dow2}.eph","$GpsRefer2");
        if(! -e "$GpsRefer2"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://igs.gnsswhu.cn/pub/gps/products/${week2}/cod${week2}${dow2}.eph.Z`;
            if(! -e "cod${week}${dow}.eph.Z"){
                `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year42}/COD${week2}${dow2}.EPH.Z`;
                if(! -e "COD${week2}${dow2}.EPH.Z"){
                    die "###ERROR: Satelite erp file not find, $$GpsRefer2!\n ftp://ftp.aiub.unibe.ch/CODE/${year42}/COD${week2}${dow2}.EPH.Z\n";
                }
                rename("COD${week2}${dow2}.EPH.Z","cod${week2}${dow2}.eph.Z");
            }
            my $eph2 = "cod${week2}${dow2}.eph.Z";
            `$unzip -cd $eph2 > "cod${week2}${dow2}.eph"`;
            `rm "cod${week2}${dow2}.eph.Z"`;
            copy("cod${week2}${dow2}.eph","${dirsp3}/");
            rename("cod${week2}${dow2}.eph","$GpsRefer2");
        }
    }
    if(! -e "$GpsRefer1"){
        copy("${dirsp3}/cod${week1}${dow1}.eph","./");
        rename("cod${week1}${dow1}.eph","$GpsRefer1");
        if(! -e "$GpsRefer1"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://igs.gnsswhu.cn/pub/gps/products/${week1}/cod${week1}${dow1}.eph.Z`;
            if(! -e "cod${week}${dow}.eph.Z"){
                `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year41}/COD${week1}${dow1}.EPH.Z`;
                if(! -e "COD${week1}${dow1}.EPH.Z"){
                    die "###ERROR: Satelite erp file not find, $GpsRefer1!\n ftp://ftp.aiub.unibe.ch/CODE/${year41}/COD${week1}${dow1}.EPH.Z\n";
                }
                rename("COD${week1}${dow1}.EPH.Z","cod${week1}${dow1}.eph.Z");
            }
            my $eph1 = "cod${week1}${dow1}.eph.Z";
            `$unzip -cd $eph1 > "cod${week1}${dow1}.eph"`;
            `rm "cod${week1}${dow1}.eph.Z"`;
            copy("cod${week1}${dow1}.eph","${dirsp3}/");
            rename("cod${week1}${dow1}.eph","$GpsRefer1");
        }
    }

    ##### mergesp3 to meger sp3 file  ######
    system("$mergesp3","$GpsRefer2","$GpsRefer","$GpsRefer1","mer${GpsRefer}")==0
        or die "###ERROR:$mergesp3 $GpsRefer2 $GpsRefer $GpsRefer1 mer${GpsRefer}\n";
    rename("mer${GpsRefer}","$GpsRefer");
    unlink("$GpsRefer2");
    unlink("$GpsRefer1");
    print "$sp3orb $GpsRefer -cfg $ctrl_file";
    system("$sp3orb","$GpsRefer","-cfg","$ctrl_file")==0 or die "###ERROR:$sp3orb $GpsRefer -cfg $ctrl_file\n";

    # Make precise satellite clock bias file
    if(! -e "$GpsSatclk"){
        #copy("${dirsp3}/cod${week}${dow}.clk","./");
        #rename("cod${week}${dow}.clk","$GpsSatclk");
        if(! -e "$GpsSatclk"){
            `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://igs.gnsswhu.cn/pub/gps/products/${week}/cod${week}${dow}.clk.Z`;
            if(! -e "cod${week}${dow}.clk.Z"){
                `$wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}${dow}.CLK.Z`;
                if(! -e 'COD${week}${dow}.CLK.Z'){
                    die "###ERROR: Satelite clk file not find, $GpsSatclk!\n ftp://ftp.aiub.unibe.ch/CODE/${year4}/COD${week}${dow}.CLK.Z\n";
                }
                rename("COD${week}${dow}.CLK.Z","cod${week}${dow}.clk.Z");
            }
            my $clk = "cod${week}${dow}.clk.Z";
            `$unzip -cd $clk > "cod${week}${dow}.clk"`;
            `rm "cod${week}${dow}.clk.Z"`;
            copy("cod${week}${dow}.clk","${dirsp3}/");
            rename("cod${week}${dow}.clk","$GpsSatclk");
        }
        rename("$GpsSatclk","sck_${year4}${doy}");
    }
}
