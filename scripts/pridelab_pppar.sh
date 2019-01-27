#!/bin/bash

#####################################################################
##                                                                 ##
##  PURPOSE: GPS PPP&PPP-AR data processing with PRIDE-PPPAR       ##
##                                                                 ##
##  AUTHOR : Yuanxin Pan    yxpan@whu.edu.cn                       ##
##                                                                 ##
##  VERSION: ver 1.00       Jan-25-2019                            ##
##                                                                 ##
##  DATE   : Jan-25, 2019                                          ##
##                                                                 ##
##              @ GNSS RESEARCH CENTER, WUHAN UNIVERSITY, 2018     ##
##                                                                 ##
#####################################################################


######################################################################
##                        Message Colors                            ##
######################################################################
NC='\033[0m'
RED='\033[0;31m'
BLUE='\033[0;34m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'

MSGERR="${RED}error:$NC"
MSGWAR="${YELLOW}warning: $NC"
MSGINF="${BLUE}:: $NC"

######################################################################
##                     Funciton definations                         ##
######################################################################
main()
{
    CheckCmdArgs "$@" || exit 1

    local ctrl_file="$1"
    local ymd_start=(${2:0:4} ${2:4:2} ${2:6:2})
    local ymd_end=(${3:0:4} ${3:4:2} ${3:6:2})
    local AR="${4:0:1}"

    # Output processing infomation
    printf "$MSGINF Processing time range: %4d-%02d-%02d  %4d-%02d-%02d\n" ${ymd_start[*]} ${ymd_end[*]}
    printf "$MSGINF Control file: %s\n" "$ctrl_file"
    printf "$MSGINF AR option: %s\n" ${AR}

    # Copy a temporary control_file
    local tmp_file=$(mktemp -u)
    cp "$ctrl_file" "$tmp_file"
    ctrl_file=$tmp_file && unset tmp_file

    # Processing day-by-day
    readonly local mjd_start=$(ymd2mjd ${ymd_start[*]})
    readonly local mjd_end=$(ymd2mjd ${ymd_end[*]})
    local work_dir=$(pwd)
    for mjd in $(seq $mjd_start $mjd_end)
    do
        cd "${work_dir}"
        mkdir -p ./${year}${doy}
        cd ./${year}/${doy} 
        if [ $? -eq 0 ]; then
            ProcessSingleDay $mjd "$ctrl_file"
        else
            printf "$MSGERR no such directory: ${work_dir}/$year/$doy\n"
            printf "$MSGWAR skip processing: $year $doy\n"
            # continue
        fi
    done

    # Done
    rm "$ctrl_file"
    printf "PRIDE-PPPAR executed successfully\n"
}

CheckCmdArgs() { #
                 #
                 # usage: CheckCmdArgs "$@"
    if [ $# -ne 4 ]; then
        PRIDE_PPPAR_Help
        return 1 
    else
        local ctrl_file="$1"
        local ymd_start="$2"
        local ymd_end="$3"
        local AR="$4"
        if [ ! -e "$ctrl_file" ]; then
            echo -e "$MSGERR no control file: $ctrl_file"
            return 1
        elif [ ${#ymd_start} -ne 8 ]; then
            echo -e "$MSGERR bad time_start format: $ymd_start"
            echo -e "$MSGINF time_start format: YYYYMMDD"
            return 1
        elif [ ${#ymd_end} -ne 8 ]; then
            echo -e "$MSGERR bad time_start format: $ymd_start"
            echo -e "$MSGINF time_end format: YYYYMMDD"
            return 1
        elif [ ${AR,,} != y -a ${AR,,} != n ]; then
            echo -e "$MSGERR unknown AR option: $AR"
            echo -e "$MSGINF AR option: Y(y)/N(n)"
            return 1
        fi
    fi
}

PRIDE_PPPAR_Help()
{
    echo " -----------------------------------------------------------------------"
    echo "  Purpose  :      GPS PPP&PPP-AR data processing with PRIDE-PPPAR"
    echo "  Copyright:      GNSS Research Center, Wuhan University, 2018"
    echo "  Usage    :      pridelab_pppar ctrl_file YYYYMMDD(Start) YYYYMMDD(End) AR/FR"
    echo "  Example  :      pridelab_pppar ses.ppp 20160101 20160101 AR/FR"
    echo "                  FR   ----- Flot Resolution"
    echo "                  AR   ----- Ambiguity Resolution"
    echo " -----------------------------------------------------------------------"
}

# printf "$MSGINF PrepareProducts: $products_dir\n"
# PrepareProducts $(ymd2mjd 16 1 1) "$products_dir" "$ctrl_file" || printf "${MSGERR} PrepareProducts failed\n"

ProcessSingleDay() { #
                     #
                     # usage: ProcessSingleDay mjd ctrl_file
    local mjd=$1
    local ctrl_file="$2"

    local ydoy=($(mjd2ydoy $mjd))
    local year=${ydoy[0]}
    local doy=${ydoy[1]}
    local ymd=($(ydoy2ymd ${ydoy[*]}))
    local mon=${ymd[1]}
    local day=${ymd[2]}
    unset ydoy ymd

    # Copy local ctrl_file
    cp -f "$ctrl_file" .
    ctrl_file=$(basename "$ctrl_file")

    # Create ctrl_file for current day
    sed -i -e "/^Session time/s/-YYYY-/$year/g" \
           -e "/^Session time/s/-MM-/$mon/g" \
           -e "/^Session time/s/-DD-/$day/g" "$ctrl_file"
    sed -i "/Rinex directory/s/-YEAR-/$year/g; s/-DOY-/$doy/g" "$ctrl_file"

    # Prepare products & tables
    local table_dir=$(get_ctrl "$ctrl_file" "Table directory")
    local product_dir=$(get_ctrl "$ctrl_file" "Sp3 directory")
    # local rinex_dir=$(get_ctrl "ctrl_file" "Rinex directory")
    # rinex_dir=$(sed "s/-YEAR-/$year/g; s/-DOY-/$doy/g" <<< "$rinex_dir")

    CopyTables "$table_dir"
    PrepareProducts $mjd "$product_dir" ${ctrl_file}

    sites=($(awk '/^ \w{4} [FKS]/ {print $1}' "$ctrl_file"))
    for site in ${sites[*]}
    do
        ProcessSingleSite "$site" $year $doy "${ctrl_file}" $AR
        [ $? -ne 0 ] && printf "$MSGWAR ProcessSingleDay: skip $year $doy $site"
    done
    rm -f ${ctrl_file}
}

ProcessSingleSite() { #
                      #
                      # usage: ProcessSingleSite site year doy ctrl_file AR(Y/N)
    local site=$1
    local year=$2
    local doy=$3
    local ctrl_file=$4
    local AR=${5:0:1}
    local rinex_dir=$(get_ctrl "$ctrl_file" "Rinex directory")
    
    # Data format conversion
    local rinexobs="${rinex_dir}/${site}${doy}0.${year:2:2}o"

    # Prepare initial sites' position
    # ...

    # Data preprocess
    local rinexnav="${rinex_dir}/brdc${doy}0.${year:2:2}n"
    local interval=$(get_ctrl "$ctrl_file" "Interval")
    local xyz=($(sed -n "/$site/ s/$site//p" "./sit.xyz"))
    if [ ${#xyz[@]} -ne 3 ]; then
         printf "$MSGERR ProcessSingleSite: no position: $site\n"
         return 1
    fi
    local positon_mode=$(grep $site "$ctrl_file" | awk '{print $2}') # Static/Kinematic/Fixed
    local cutoff_elev=$(grep $site "$ctrl_file" | awk '{print $5}')  # int, degree

    local rhd_file="rhd_${year}${doy}_${site}"
    local ymd=$(ydoy2ymd $year $doy)
    local cmd=""
    if [ $positon_mode == S -o $positon_mode == F ]; then
        cmd="tedit ${rinexobs} -int ${interval} -rnxn ${rinexnav} -xyz ${xyz[*]} \
            -len 86400 -short 1200 -lc_check only -rhd ${rhd_file} -pc_check 300 \
            -elev ${cutoff_elev} -time ${ymd[*]} 0 0 0"
    elif [ $positon_mode == K ]; then
        cmd="tedit ${rinexobs} -int ${interval} -rnxn ${rinexnav} -xyz ${xyz[*]} \
            -short 1 -lc_check no -rhd ${rhd_file} -len 86400 -time ${ymd[*]} 0 0 0"
    else 
        printf "$MSGERR ProcessSingleSite: unknown position mode: $site $positon_mode\n"
        return 1
    fi
     ExecuteWithoutOutput "$cmd" || return 1

    # Create site ctrl_file
    local tmp_ctrl=$(basename `mktemp -u`)
    sed '/^\*NAME/ q' $ctrl_file > ${tmp_ctrl}
    grep "^ ${site} [FKS]" "$ctrl_file" >> ${tmp_ctrl}
    echo "-Station used" >> ${tmp_ctrl}

    # Data clean (iteration)
    local short=$((600/$interval))
    local jumps=(400 200 100 50 50) # (500 300 200 100 50)
    for jump in ${jumps[*]}
    do
        cmd="lsq ${tmp_ctrl}" && ExecuteWithoutOutput "$cmd"
        [ $? -ne 0 ] && return 1
        cmd="redig res_${year}${doy} -jmp $jump -sht $short" && ExecuteWithoutOutput "$cmd"
        [ $? -ne 0 ] && return 1
    done
    # cmd="redig res_${year}${doy}" && Execute "$cmd" && \

    # Final process
    cmd="lsq ${tmp_ctrl}" && Execute "$cmd"
    if [ $AR == Y -o $AR == y ]; then
        cmd="arsig ${tmp_ctrl}" && Execute "$cmd"
        cmd="lsq ${tmp_ctrl}" && Execute "$cmd"
    fi

    # Rename result files
    local tp types=(kin pos rck ztd htg amb res stt con) fn
    for tp in ${types[*]}
    do
        fn=${tp}_${year}${doy}
        [ -f ${fn} ] && mv -f ${fn} ${fn}_${site}
    done
    rm -f ${tmp_ctrl}
}

CopyTables() { #
               #
               # usage: CopyTables table_dir
    local table_dir="$1"
    local tables=(file_name abs_igs.atx jpleph_de405 leap.sec oceanload sit.xyz)
    for table in ${tables[*]}
    do
        if [ ! -f "$table_dir/$table" ]; then
             printf "$MSGERR CopyTables: no such file: $table_dir/$table\n"
             exit 1
        fi
        cp -f "$table_dir/$table" .
    done
}

PrepareProducts() { #
                    #
                    # usage: PrepareProducts mjd products_dir
    local mjd_mid=$1
    local products_dir="$2"
    local ctrl_file="$3"
    [ -d $products_dir ] || mkdir -p "$products_dir"

    local ydoy=($(mjd2ydoy $mjd_mid))
    local ymd=($(ydoy2ymd ${ydoy[*]}))
    local wkdow=($(mjd2wkdow $mjd_mid))
    local year=${ydoy[0]}
    # echo ${ydoy[*]} ${ymd[*]} ${wkdow[*]}

    local clk="cck${wkdow[0]}${wkdow[1]}.clk"
    local clk_url="ftp://192.168.111.19/${clk}"
    CopyOrDownloadProduct "$products_dir/$clk" "$clk_url" || return 1

    local fcb="WHU0IGSFIN_${year}${ydoy[1]}0000_01D_01D_ABS.BIA";
    local fcb_url="ftp://192.168.111.19/${fcb}"
    CopyOrDownloadProduct "$products_dir/$fcb" "$fcb_url" || return 1

    local dcb1="P1C1${year:2:2}${ymd[1]}_RINEX.DCB.Z"
    local dcb1url="ftp://ftp.aiub.unibe.ch/CODE/${year}/${dcb1}"
    CopyOrDownloadProduct "$products_dir/$dcb1" "$dcb1url" || return 1
    uncompress -f ${dcb1}

    local dcb2="P2C2${year:2:2}${ymd[1]}_RINEX.DCB.Z"
    local dcb2url="ftp://ftp.aiub.unibe.ch/CODE/${year}/${dcb2}"
    CopyOrDownloadProduct "$products_dir/$dcb2" "$dcb2url" || return 1
    uncompress -f ${dcb2}

    erp="COD${wkdow[0]}${wkdow[1]}.ERP.Z"
    erp_url="ftp://ftp.aiub.unibe.ch/CODE/${ydoy[0]}/${erp}"
    CopyOrDownloadProduct "$products_dir/$erp" "$erp_url" || return 1
    uncompress -f ${erp}

    local sp3s tmpy
    local sp3 sp3_url i=0
    for mjd in $((mjd_mid-1)) mjd_mid $((mjd_mid+1))
    do
        tmpy=($(mjd2ydoy $mjd))
        # ymd=($(ydoy2ymd ${ydoy[*]}))
        wkdow=($(mjd2wkdow $mjd))

        sp3="COD${wkdow[0]}${wkdow[1]}.EPH.Z"
        sp3_url="ftp://ftp.aiub.unibe.ch/CODE/${tmpy}/${sp3}"
        CopyOrDownloadProduct "$products_dir/$sp3" "$sp3_url" || return 1
        uncompress -f ${sp3}
        sp3s[$((i++))]="COD${wkdow[0]}${wkdow[1]}.EPH"
    done

    # rename products
    mv ${clk} sck_${ydoy[0]}${ydoy[1]}
    mv ${fcb} fcb_${ydoy[0]}${ydoy[1]}
    mv ${dcb1:0:18} P1C1.dcb
    mv ${dcb2:0:18} P2C2.dcb
    mv ${erp:0:12} igserp

    # Generate binary sp3
    local cmd="mergesp3 ${sp3s[*]} orb_temp" && Execute "${cmd}"
    cmd="sp3orb orb_temp -cfg ${ctrl_file}" && Execute "${cmd}" && rm -f orb_temp
}

CopyOrDownloadProduct() { #
               #
               # usage: CopyOrDownloadProduct file url
    local file="$1"
    local url="$2"
    if [ -f "$file" ]; then
        cp -f "$file" .
    else
        WgetDownload "$url" || return 1
        cp $(basename "$url") "$file"
    fi
}

WgetDownload() { #
                 #
                 # usage: WgetDownload url
    local url="$1"
    local args="-nv -nc -c -t 3 --connect-timeout=10 --read-timeout=60"
    cmd="wget ${args} ${url}"
    $cmd
    [ -e $(basename "${url}") ] && return 0 || return 1
}

UncompressFile() { #
                   #
                   # usage: UncompressFile file del(Y/N)
    local file="$1"
    local del=$2
    # file $file
}

ymd2mjd()
{
    local year=$1
    local mon=$2
    local day=$3
    [ $year -lt 100 ] && year=$((year+2000))
    if [ $mon -le 2 ];then
        mon=$(($mon+12))
        year=$(($year-1))
    fi
    local mjd=$(bc <<< "$year*365.25 - $year*365.25 % 1 - 679006")
    mjd=$(bc <<< "($mjd + (30.6001*($mon+1))/1 + 2 - $year/100 + $year/400 + $day)/1")
    echo $mjd
}

mjd2ydoy()
{
    local mjd=$1
    local year=$((($mjd + 678940)/365))
    local mjd0=$(ymd2mjd $year 1 1)
    local doy=$(($mjd-$mjd0))
    while [ $doy -le 0 ];do
        year=$(($year-1))
        mjd0=$(ymd2mjd $year 1 1)
        doy=$(($mjd-$mjd0+1))
    done
    printf "%d %03d" $year $doy
}

ymd2wkdow()
{
    local year=$1
    local mon=$2
    local day=$3
    local mjd0=44243
    local mjd=$(ymd2mjd $year $mon $day)
    local difmjd=$(($mjd-$mjd0-1))
    local week=$(($difmjd/7))
    local dow=$(($difmjd%7))
    echo $week $dow
}

mjd2wkdow()
{
    local mjd=$1
    local mjd0=44243
    local difmjd=$(($mjd-$mjd0-1))
    local week=$(($difmjd/7))
    local dow=$(($difmjd%7))
    echo $week $dow
}

ydoy2ymd()
{
    local iyear=$1
    local idoy=$2
    local days_in_month=(31 28 31 30 31 30 31 31 30 31 30 31)
    local iday=0
    [ $iyear -lt 100 ] && iyear=$((iyear+2000))
    local tmp1=$(($iyear%4))
    local tmp2=$(($iyear%100))
    local tmp3=$(($iyear%400))
    if [ $tmp1 -eq 0 -a $tmp2 -ne 0 ] || [ $tmp3 -eq 0 ];then
       let "days_in_month[1]=29"
    fi
    local id=$idoy
    local imon=0
    local days
    for days in ${days_in_month[*]}
    do
        let "id=$id-$days"
        let "imon=$imon+1"
        if [ $id -gt 0 ];then
            continue
        fi
        let "iday=$id + $days"
        break;
    done
    printf "%d %02d %02d" $iyear $imon $iday
} 

Execute()
{
    local cmd="$1"
    # echo $cmd
    time=`date -u +'%Y-%m-%d %H:%M:%S'`
    $cmd
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}($time)${NC} ${BLUE}$cmd${NC} exited successfully" # | tee -a $log
        return 0
    else
        echo -e "${GREEN}($time)${NC} ${BLUE}$cmd${NC} exited failed"  #| tee -a $log
        return 1
    fi
}

ExecuteWithoutOutput()
{
    local cmd="$1"
    # echo $cmd
    $cmd > /dev/null 2>&1
    time=`date -u +'%Y-%m-%d %H:%M:%S'`
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}($time)${NC} ${BLUE}$cmd${NC} exited successfully" # | tee -a $log
        return 0
    else
        echo -e "${GREEN}($time)${NC} ${BLUE}$cmd${NC} exited failed"  #| tee -a $log
        return 1
    fi
}

main "$@"

