      ****************************************************************
      *    COPY-MEMBER: DRAT0146  KONSTANTEN FÜR PGV    KTY = 115    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Fr.Schrieber                                    *
      *    DATUM:    12.08.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG       ! DATUM  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY         !12.08.97!NEUERSTELLUNG             ! Schrieber*
      *               !        !                          !          *
      * APPA0039      !10.05.99!ERGÄNZEN PGV 4015         !PROEMPELER*
      * APPA0086      !01.03.00!EINFUEHRUNG NEUE PGV FUER !WILKENS   *
      *               !        !AMV IN PEG                !          *
      * APA00090      !13.04.00!NEUE BADENIA PGV'S        !MUND      *
      *               !        !                          !          *
      * APA00105      !25.09.00!NEUE PGV's iii            !Jäger     *
      * APA00126      !02.03.01!Neuer PGV 1014 VL-Anschluß!Brenner   *
      * APA00133      !18.07.01!NEUER PGV 1060 ADVOCARD   !KLEEMEIER *
      * APA00133      !03.09.01!PGV 2999/3999 HINZU       !KLEEMEIER *
      * APA00162      !30.10.01!PGV 4045 HINZU            !KLEEMEIER *
      * APA00206      !21.08.02!PGV 3025 HINZU            !MUND      *
      * APA00330      !27.12.04!PGV 2013 Hinzu            !Jäger     *
      * APA00385      !07.10.05!PGV 1035 Hinzu            !Jäger     *
      * APA00495      !02.10.06!PGV 2038 und 1068 hinzu   !Jäger     *
      *               !12.10.06!PGV 3026 neu              !Jäger     *
      * APA00638      !10.07.07!PGV 3013 NEU              !SCHEIDEL  *
      * APA00725      !19.01.09!PGV 1017 NEU              !SCHEIDEL  *
      * APA00940      !08.09.09!PGV 1023 NEU              !BUCH      *
      * APA01259      !16.09.11!PGV 2070 NEU              !BUCH      *
      * APA01246      !19.09.11!PGV 2060 NEU              !BUCH      *
      * APA01310      !22.06.12!NEUSTORNO                 !SCHEIDEL  *
      * APA01465      !08.10.12!PGV 4014 NEU              !BUCH      *
      * APA01550      !22.03.13!NEUE PGV'S AML            !WANG      *
      *               !20.01.13!NEUE PGV'S AML            !WANG      *
      * APA01827      !11.08.14!CKV:PGV 4060,4061,4065 neu!STRAUSS   *
      * APA01980      !15.01.15!AML:PGV 1018,4018 NEU     !BUCH      *
      * APA02020      !30.04.15!BADENIA PGV 4034 NEU      !BUCH      *
      * APA02038      !02.05.15!AML: PGV 1941             !BUCH      *
      * APA02019      !02.09.15!AML: PGV 1011             !BUCH      *
      * APA01932      !19.05.15!LUCY: PGV'S 1052, 4052    !KÖPRÜCÜ   *
      * APA01932      !23.11.15!AML PGV 1011 wieder raus  !KÖPRÜCÜ   *
      * APA02169      !24.11.15!NEUE PGV's FÜR CKV        !KÖPRÜCÜ   *
      * APA02151      !29.06.16!VERTEILTE PROV LEBEN      !SCHEIDEL  *
      * APA02311      !15.11.16!1011 WIEDER HINZU         !SCHEIDEL  *
      * APA02363      !        !2.42 in neuer Stückliste  !BUCH      *
      * APA02347      !16.01.17!NEUER nPGV 1071 BKK-LINDE !KÖPRÜCÜ   *
      * APA02353      !02.03.17!NEUE PGV'S FÜR TELESON    !WANG      *
      * APA02438      !15.05.17!NEUE PGV'S FÜR DEVOLO     !KRANZ     *
      * APA02396      !12.06.17!NEUE PGV'S FÜR Lohnwerk   !KRANZ     *
      * APA02473      !24.08.17!NEUE PGV'S FÜR Cadooz     !Buch      *
      * APA02508      !08.09.17!NEUE PGV'S FÜR AML        !Köprücü   *
      * APA02579      !24.04.18!NEUE PGV'S FÜR GEL        !SCHEIDEL  *
      * APA02797      !21.01.19!NEUER PGV 2041            !KRANZ     *
      * APA02830      !12.03.19!NEUER PGV 1024            !KRANZ     *
      * APA02838      !03.09.19!FEHLENDEN PGV 1015 HINZU  !KÖPRÜCÜ   *
      * APA02885      !25.09.19!NEUE PGV 1026,2026,3026   !BUCH      *
      *               !        !         4026             !          *
      * APA02880      !22.11.19!NEUE PGV 1072,1073,2073   !BUCH      *
      *               !        !         3073,4073        !          *
      * APA02899      !16.12.19!NEUE PGV 1053,1054        !SCHEIDEL  *
      * APA02944      !29.07.20!NEUE PGV 1066,1061 (CKV)  !KÖPRÜCÜ   *
      * DSY03269      !19.01.21!NEUER PGV 1029            !SCHEIDEL  *
      * DSY02385      !18.02.21!NEUER PGV 1032 (AdvoCard) !KÖPRÜCÜ   *
      * DSY04569      !12.05.22!NEU X062, 1063, 1034      !SCHEIDEL  *
      *               !        !                          !          *
      *               !        !                          !          *
      *               !        !                          !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PGV-0100           PIC S9(004) BINARY
                                                   VALUE  0100.
           05 K-KNR-PGV-0102           PIC S9(004) BINARY
                                                   VALUE  0102.
           05 K-KNR-PGV-0103           PIC S9(004) BINARY
                                                   VALUE  0103.
           05 K-KNR-PGV-0104           PIC S9(004) BINARY
                                                   VALUE  0104.
           05 K-KNR-PGV-1000           PIC S9(004) BINARY
                                                   VALUE  1000.
           05 K-KNR-PGV-1001           PIC S9(004) BINARY
                                                   VALUE  1001.
           05 K-KNR-PGV-1002           PIC S9(004) BINARY
                                                   VALUE  1002.
           05 K-KNR-PGV-1003           PIC S9(004) BINARY
                                                   VALUE  1003.
           05 K-KNR-PGV-1004           PIC S9(004) BINARY
                                                   VALUE  1004.
           05 K-KNR-PGV-1005           PIC S9(004) BINARY
                                                   VALUE  1005.
           05 K-KNR-PGV-1006           PIC S9(004) BINARY
                                                   VALUE  1006.
           05 K-KNR-PGV-1007           PIC S9(004) BINARY
                                                   VALUE  1007.
           05 K-KNR-PGV-1008           PIC S9(004) BINARY
                                                   VALUE  1008.
           05 K-KNR-PGV-1009           PIC S9(004) BINARY
                                                   VALUE  1009.
           05 K-KNR-PGV-1010           PIC S9(004) BINARY
                                                   VALUE  1010.
           05 K-KNR-PGV-1011           PIC S9(004) BINARY
                                                   VALUE  1011.
           05 K-KNR-PGV-1012           PIC S9(004) BINARY
                                                   VALUE  1012.
           05 K-KNR-PGV-1013           PIC S9(004) BINARY
                                                   VALUE  1013.
           05 K-KNR-PGV-1014           PIC S9(004) BINARY
                                                   VALUE  1014.
           05 K-KNR-PGV-1015           PIC S9(004) BINARY
                                                   VALUE  1015.
           05 K-KNR-PGV-1016           PIC S9(004) BINARY
                                                   VALUE  1016.
           05 K-KNR-PGV-1017           PIC S9(004) BINARY
                                                   VALUE  1017.
           05 K-KNR-PGV-1018           PIC S9(004) BINARY
                                                   VALUE  1018.
           05 K-KNR-PGV-1020           PIC S9(004) BINARY
                                                   VALUE  1020.
           05 K-KNR-PGV-1021           PIC S9(004) BINARY
                                                   VALUE  1021.
           05 K-KNR-PGV-1022           PIC S9(004) BINARY
                                                   VALUE  1022.
           05 K-KNR-PGV-1023           PIC S9(004) BINARY
                                                   VALUE  1023.
           05 K-KNR-PGV-1024           PIC S9(004) BINARY
                                                   VALUE  1024.
           05 K-KNR-PGV-1025           PIC S9(004) BINARY
                                                   VALUE  1025.
           05 K-KNR-PGV-1026           PIC S9(004) BINARY
                                                   VALUE  1026.
           05 K-KNR-PGV-1029           PIC S9(004) BINARY
                                                   VALUE  1029.
           05 K-KNR-PGV-1030           PIC S9(004) BINARY
                                                   VALUE  1030.
           05 K-KNR-PGV-1031           PIC S9(004) BINARY
                                                   VALUE  1031.
           05 K-KNR-PGV-1032           PIC S9(004) BINARY
                                                   VALUE  1032.
           05 K-KNR-PGV-1033           PIC S9(004) BINARY
                                                   VALUE  1033.
           05 K-KNR-PGV-1034           PIC S9(004) BINARY
                                                   VALUE  1034.
           05 K-KNR-PGV-1035           PIC S9(004) BINARY
                                                   VALUE  1035.
           05 K-KNR-PGV-1036           PIC S9(004) BINARY
                                                   VALUE  1036.
           05 K-KNR-PGV-1040           PIC S9(004) BINARY
                                                   VALUE  1040.
           05 K-KNR-PGV-1044           PIC S9(004) BINARY
                                                   VALUE  1044.
           05 K-KNR-PGV-1050           PIC S9(004) BINARY
                                                   VALUE  1050.
           05 K-KNR-PGV-1051           PIC S9(004) BINARY
                                                   VALUE  1051.
           05 K-KNR-PGV-1052           PIC S9(004) BINARY
                                                   VALUE  1052.
           05 K-KNR-PGV-1053           PIC S9(004) BINARY
                                                   VALUE  1053.
           05 K-KNR-PGV-1054           PIC S9(004) BINARY
                                                   VALUE  1054.
           05 K-KNR-PGV-1055           PIC S9(004) BINARY
                                                   VALUE  1055.
           05 K-KNR-PGV-1056           PIC S9(004) BINARY
                                                   VALUE  1056.
           05 K-KNR-PGV-1060           PIC S9(004) BINARY
                                                   VALUE  1060.
           05 K-KNR-PGV-1061           PIC S9(004) BINARY
                                                   VALUE  1061.
           05 K-KNR-PGV-1062           PIC S9(004) BINARY
                                                   VALUE  1062.
           05 K-KNR-PGV-1063           PIC S9(004) BINARY
                                                   VALUE  1063.
           05 K-KNR-PGV-1064           PIC S9(004) BINARY
                                                   VALUE  1064.
           05 K-KNR-PGV-1066           PIC S9(004) BINARY
                                                   VALUE  1066.
           05 K-KNR-PGV-1068           PIC S9(004) BINARY
                                                   VALUE  1068.
           05 K-KNR-PGV-1070           PIC S9(004) BINARY
                                                   VALUE  1070.
           05 K-KNR-PGV-1071           PIC S9(004) BINARY
                                                   VALUE  1071.
           05 K-KNR-PGV-1072           PIC S9(004) BINARY
                                                   VALUE  1072.
           05 K-KNR-PGV-1073           PIC S9(004) BINARY
                                                   VALUE  1073.
           05 K-KNR-PGV-1080           PIC S9(004) BINARY
                                                   VALUE  1080.
           05 K-KNR-PGV-1085           PIC S9(004) BINARY
                                                   VALUE  1085.
           05 K-KNR-PGV-1086           PIC S9(004) BINARY
                                                   VALUE  1086.
           05 K-KNR-PGV-1087           PIC S9(004) BINARY
                                                   VALUE  1087.
           05 K-KNR-PGV-1094           PIC S9(004) BINARY
                                                   VALUE  1094.
           05 K-KNR-PGV-1095           PIC S9(004) BINARY
                                                   VALUE  1095.
           05 K-KNR-PGV-1096           PIC S9(004) BINARY
                                                   VALUE  1096.
           05 K-KNR-PGV-1097           PIC S9(004) BINARY
                                                   VALUE  1097.
           05 K-KNR-PGV-1099           PIC S9(004) BINARY
                                                   VALUE  1099.
           05 K-KNR-PGV-1119           PIC S9(004) BINARY
                                                   VALUE  1119.
           05 K-KNR-PGV-1199           PIC S9(004) BINARY
                                                   VALUE  1199.
           05 K-KNR-PGV-1900           PIC S9(004) BINARY
                                                   VALUE  1900.
           05 K-KNR-PGV-1902           PIC S9(004) BINARY
                                                   VALUE  1902.
           05 K-KNR-PGV-1903           PIC S9(004) BINARY
                                                   VALUE  1903.
           05 K-KNR-PGV-1905           PIC S9(004) BINARY
                                                   VALUE  1905.
           05 K-KNR-PGV-1912           PIC S9(004) BINARY
                                                   VALUE  1912.
           05 K-KNR-PGV-1913           PIC S9(004) BINARY
                                                   VALUE  1913.
           05 K-KNR-PGV-1919           PIC S9(004) BINARY
                                                   VALUE  1919.
           05 K-KNR-PGV-1920           PIC S9(004) BINARY
                                                   VALUE  1920.
           05 K-KNR-PGV-1921           PIC S9(004) BINARY
                                                   VALUE  1921.
           05 K-KNR-PGV-1922           PIC S9(004) BINARY
                                                   VALUE  1922.
           05 K-KNR-PGV-1927           PIC S9(004) BINARY
                                                   VALUE  1927.
           05 K-KNR-PGV-1930           PIC S9(004) BINARY
                                                   VALUE  1930.
           05 K-KNR-PGV-1940           PIC S9(004) BINARY
                                                   VALUE  1940.
           05 K-KNR-PGV-1941           PIC S9(004) BINARY
                                                   VALUE  1941.
           05 K-KNR-PGV-1942           PIC S9(004) BINARY
                                                   VALUE  1942.
           05 K-KNR-PGV-1943           PIC S9(004) BINARY
                                                   VALUE  1943.
           05 K-KNR-PGV-1944           PIC S9(004) BINARY
                                                   VALUE  1944.
           05 K-KNR-PGV-1946           PIC S9(004) BINARY
                                                   VALUE  1946.
           05 K-KNR-PGV-1953           PIC S9(004) BINARY
                                                   VALUE  1953.
           05 K-KNR-PGV-1958           PIC S9(004) BINARY
                                                   VALUE  1958.
           05 K-KNR-PGV-1960           PIC S9(004) BINARY
                                                   VALUE  1960.
           05 K-KNR-PGV-1962           PIC S9(004) BINARY
                                                   VALUE  1962.
           05 K-KNR-PGV-1963           PIC S9(004) BINARY
                                                   VALUE  1963.
           05 K-KNR-PGV-1964           PIC S9(004) BINARY
                                                   VALUE  1964.
           05 K-KNR-PGV-1967           PIC S9(004) BINARY
                                                   VALUE  1967.
           05 K-KNR-PGV-1968           PIC S9(004) BINARY
                                                   VALUE  1968.
           05 K-KNR-PGV-1970           PIC S9(004) BINARY
                                                   VALUE  1970.
           05 K-KNR-PGV-1972           PIC S9(004) BINARY
                                                   VALUE  1972.
           05 K-KNR-PGV-1977           PIC S9(004) BINARY
                                                   VALUE  1977.
           05 K-KNR-PGV-1978           PIC S9(004) BINARY
                                                   VALUE  1978.
           05 K-KNR-PGV-1979           PIC S9(004) BINARY
                                                   VALUE  1979.
           05 K-KNR-PGV-1980           PIC S9(004) BINARY
                                                   VALUE  1980.
           05 K-KNR-PGV-1982           PIC S9(004) BINARY
                                                   VALUE  1982.
           05 K-KNR-PGV-1983           PIC S9(004) BINARY
                                                   VALUE  1983.
           05 K-KNR-PGV-1984           PIC S9(004) BINARY
                                                   VALUE  1984.
           05 K-KNR-PGV-1985           PIC S9(004) BINARY
                                                   VALUE  1985.
           05 K-KNR-PGV-1987           PIC S9(004) BINARY
                                                   VALUE  1987.
           05 K-KNR-PGV-1988           PIC S9(004) BINARY
                                                   VALUE  1988.
           05 K-KNR-PGV-1989           PIC S9(004) BINARY
                                                   VALUE  1989.
           05 K-KNR-PGV-1990           PIC S9(004) BINARY
                                                   VALUE  1990.
           05 K-KNR-PGV-1992           PIC S9(004) BINARY
                                                   VALUE  1992.
           05 K-KNR-PGV-1997           PIC S9(004) BINARY
                                                   VALUE  1997.
           05 K-KNR-PGV-1998           PIC S9(004) BINARY
                                                   VALUE  1998.
           05 K-KNR-PGV-1999           PIC S9(004) BINARY
                                                   VALUE  1999.
           05 K-KNR-PGV-2001           PIC S9(004) BINARY
                                                   VALUE  2001.
           05 K-KNR-PGV-2002           PIC S9(004) BINARY
                                                   VALUE  2002.
           05 K-KNR-PGV-2003           PIC S9(004) BINARY
                                                   VALUE  2003.
           05 K-KNR-PGV-2004           PIC S9(004) BINARY
                                                   VALUE  2004.
           05 K-KNR-PGV-2005           PIC S9(004) BINARY
                                                   VALUE  2005.
           05 K-KNR-PGV-2006           PIC S9(004) BINARY
                                                   VALUE  2006.
           05 K-KNR-PGV-2007           PIC S9(004) BINARY
                                                   VALUE  2007.
           05 K-KNR-PGV-2008           PIC S9(004) BINARY
                                                   VALUE  2008.
           05 K-KNR-PGV-2010           PIC S9(004) BINARY
                                                   VALUE  2010.
           05 K-KNR-PGV-2013           PIC S9(004) BINARY
                                                   VALUE  2013.
           05 K-KNR-PGV-2019           PIC S9(004) BINARY
                                                   VALUE  2019.
           05 K-KNR-PGV-2020           PIC S9(004) BINARY
                                                   VALUE  2020.
           05 K-KNR-PGV-2022           PIC S9(004) BINARY
                                                   VALUE  2022.
           05 K-KNR-PGV-2026           PIC S9(004) BINARY
                                                   VALUE  2026.
           05 K-KNR-PGV-2030           PIC S9(004) BINARY
                                                   VALUE  2030.
           05 K-KNR-PGV-2033           PIC S9(004) BINARY
                                                   VALUE  2033.
           05 K-KNR-PGV-2038           PIC S9(004) BINARY
                                                   VALUE  2038.
           05 K-KNR-PGV-2040           PIC S9(004) BINARY
                                                   VALUE  2040.
           05 K-KNR-PGV-2041           PIC S9(004) BINARY
                                                   VALUE  2041.
           05 K-KNR-PGV-2044           PIC S9(004) BINARY
                                                   VALUE  2044.
           05 K-KNR-PGV-2050           PIC S9(004) BINARY
                                                   VALUE  2050.
           05 K-KNR-PGV-2055           PIC S9(004) BINARY
                                                   VALUE  2055.
           05 K-KNR-PGV-2060           PIC S9(004) BINARY
                                                   VALUE  2060.
           05 K-KNR-PGV-2062           PIC S9(004) BINARY
                                                   VALUE  2062.
           05 K-KNR-PGV-2070           PIC S9(004) BINARY
                                                   VALUE  2070.
           05 K-KNR-PGV-2073           PIC S9(004) BINARY
                                                   VALUE  2073.
           05 K-KNR-PGV-2080           PIC S9(004) BINARY
                                                   VALUE  2080.
           05 K-KNR-PGV-2085           PIC S9(004) BINARY
                                                   VALUE  2085.
           05 K-KNR-PGV-2086           PIC S9(004) BINARY
                                                   VALUE  2086.
           05 K-KNR-PGV-2087           PIC S9(004) BINARY
                                                   VALUE  2087.
           05 K-KNR-PGV-2900           PIC S9(004) BINARY
                                                   VALUE  2900.
           05 K-KNR-PGV-2902           PIC S9(004) BINARY
                                                   VALUE  2902.
           05 K-KNR-PGV-2903           PIC S9(004) BINARY
                                                   VALUE  2903.
           05 K-KNR-PGV-2094           PIC S9(004) BINARY
                                                   VALUE  2094.
           05 K-KNR-PGV-2905           PIC S9(004) BINARY
                                                   VALUE  2905.
           05 K-KNR-PGV-2907           PIC S9(004) BINARY
                                                   VALUE  2907.
           05 K-KNR-PGV-2908           PIC S9(004) BINARY
                                                   VALUE  2908.
           05 K-KNR-PGV-2912           PIC S9(004) BINARY
                                                   VALUE  2912.
           05 K-KNR-PGV-2913           PIC S9(004) BINARY
                                                   VALUE  2913.
           05 K-KNR-PGV-2919           PIC S9(004) BINARY
                                                   VALUE  2919.
           05 K-KNR-PGV-2920           PIC S9(004) BINARY
                                                   VALUE  2920.
           05 K-KNR-PGV-2921           PIC S9(004) BINARY
                                                   VALUE  2921.
           05 K-KNR-PGV-2922           PIC S9(004) BINARY
                                                   VALUE  2922.
           05 K-KNR-PGV-2927           PIC S9(004) BINARY
                                                   VALUE  2927.
           05 K-KNR-PGV-2925           PIC S9(004) BINARY
                                                   VALUE  2925.
           05 K-KNR-PGV-2935           PIC S9(004) BINARY
                                                   VALUE  2935.
           05 K-KNR-PGV-2942           PIC S9(004) BINARY
                                                   VALUE  2942.
           05 K-KNR-PGV-2943           PIC S9(004) BINARY
                                                   VALUE  2943.
           05 K-KNR-PGV-2944           PIC S9(004) BINARY
                                                   VALUE  2944.
           05 K-KNR-PGV-2945           PIC S9(004) BINARY
                                                   VALUE  2945.
           05 K-KNR-PGV-2946           PIC S9(004) BINARY
                                                   VALUE  2946.
           05 K-KNR-PGV-2958           PIC S9(004) BINARY
                                                   VALUE  2958.
           05 K-KNR-PGV-2980           PIC S9(004) BINARY
                                                   VALUE  2980.
           05 K-KNR-PGV-2982           PIC S9(004) BINARY
                                                   VALUE  2982.
           05 K-KNR-PGV-2983           PIC S9(004) BINARY
                                                   VALUE  2983.
           05 K-KNR-PGV-2984           PIC S9(004) BINARY
                                                   VALUE  2984.
           05 K-KNR-PGV-2985           PIC S9(004) BINARY
                                                   VALUE  2985.
           05 K-KNR-PGV-2987           PIC S9(004) BINARY
                                                   VALUE  2987.
           05 K-KNR-PGV-2988           PIC S9(004) BINARY
                                                   VALUE  2988.
           05 K-KNR-PGV-2989           PIC S9(004) BINARY
                                                   VALUE  2989.
           05 K-KNR-PGV-2990           PIC S9(004) BINARY
                                                   VALUE  2990.
           05 K-KNR-PGV-2992           PIC S9(004) BINARY
                                                   VALUE  2992.
           05 K-KNR-PGV-2997           PIC S9(004) BINARY
                                                   VALUE  2997.
           05 K-KNR-PGV-2998           PIC S9(004) BINARY
                                                   VALUE  2998.
           05 K-KNR-PGV-2999           PIC S9(004) BINARY
                                                   VALUE  2999.
           05 K-KNR-PGV-3001           PIC S9(004) BINARY
                                                   VALUE  3001.
           05 K-KNR-PGV-3002           PIC S9(004) BINARY
                                                   VALUE  3002.
           05 K-KNR-PGV-3003           PIC S9(004) BINARY
                                                   VALUE  3003.
           05 K-KNR-PGV-3004           PIC S9(004) BINARY
                                                   VALUE  3004.
           05 K-KNR-PGV-3005           PIC S9(004) BINARY
                                                   VALUE  3005.
           05 K-KNR-PGV-3006           PIC S9(004) BINARY
                                                   VALUE  3006.
           05 K-KNR-PGV-3007           PIC S9(004) BINARY
                                                   VALUE  3007.
           05 K-KNR-PGV-3008           PIC S9(004) BINARY
                                                   VALUE  3008.
           05 K-KNR-PGV-3010           PIC S9(004) BINARY
                                                   VALUE  3010.
           05 K-KNR-PGV-3013           PIC S9(004) BINARY
                                                   VALUE  3013.
           05 K-KNR-PGV-3022           PIC S9(004) BINARY
                                                   VALUE  3022.
           05 K-KNR-PGV-3025           PIC S9(004) BINARY
                                                   VALUE  3025.
           05 K-KNR-PGV-3026           PIC S9(004) BINARY
                                                   VALUE  3026.
           05 K-KNR-PGV-3033           PIC S9(004) BINARY
                                                   VALUE  3033.
           05 K-KNR-PGV-3036           PIC S9(004) BINARY
                                                   VALUE  3036.
           05 K-KNR-PGV-3044           PIC S9(004) BINARY
                                                   VALUE  3044.
           05 K-KNR-PGV-3055           PIC S9(004) BINARY
                                                   VALUE  3055.
           05 K-KNR-PGV-3062           PIC S9(004) BINARY
                                                   VALUE  3062.
           05 K-KNR-PGV-3073           PIC S9(004) BINARY
                                                   VALUE  3073.
           05 K-KNR-PGV-3080           PIC S9(004) BINARY
                                                   VALUE  3080.
           05 K-KNR-PGV-3085           PIC S9(004) BINARY
                                                   VALUE  3085.
           05 K-KNR-PGV-3086           PIC S9(004) BINARY
                                                   VALUE  3086.
           05 K-KNR-PGV-3087           PIC S9(004) BINARY
                                                   VALUE  3087.
           05 K-KNR-PGV-3900           PIC S9(004) BINARY
                                                   VALUE  3900.
           05 K-KNR-PGV-3902           PIC S9(004) BINARY
                                                   VALUE  3902.
           05 K-KNR-PGV-3903           PIC S9(004) BINARY
                                                   VALUE  3903.
           05 K-KNR-PGV-3094           PIC S9(004) BINARY
                                                   VALUE  3094.
           05 K-KNR-PGV-3905           PIC S9(004) BINARY
                                                   VALUE  3905.
           05 K-KNR-PGV-3907           PIC S9(004) BINARY
                                                   VALUE  3907.
           05 K-KNR-PGV-3908           PIC S9(004) BINARY
                                                   VALUE  3908.
           05 K-KNR-PGV-3912           PIC S9(004) BINARY
                                                   VALUE  3912.
           05 K-KNR-PGV-3913           PIC S9(004) BINARY
                                                   VALUE  3913.
           05 K-KNR-PGV-3919           PIC S9(004) BINARY
                                                   VALUE  3919.
           05 K-KNR-PGV-3920           PIC S9(004) BINARY
                                                   VALUE  3920.
           05 K-KNR-PGV-3921           PIC S9(004) BINARY
                                                   VALUE  3921.
           05 K-KNR-PGV-3931           PIC S9(004) BINARY
                                                   VALUE  3931.
           05 K-KNR-PGV-3925           PIC S9(004) BINARY
                                                   VALUE  3925.
           05 K-KNR-PGV-3935           PIC S9(004) BINARY
                                                   VALUE  3935.
           05 K-KNR-PGV-3942           PIC S9(004) BINARY
                                                   VALUE  3942.
           05 K-KNR-PGV-3943           PIC S9(004) BINARY
                                                   VALUE  3943.
           05 K-KNR-PGV-3944           PIC S9(004) BINARY
                                                   VALUE  3944.
           05 K-KNR-PGV-3945           PIC S9(004) BINARY
                                                   VALUE  3945.
           05 K-KNR-PGV-3946           PIC S9(004) BINARY
                                                   VALUE  3946.
           05 K-KNR-PGV-3958           PIC S9(004) BINARY
                                                   VALUE  3958.
           05 K-KNR-PGV-3980           PIC S9(004) BINARY
                                                   VALUE  3980.
           05 K-KNR-PGV-3982           PIC S9(004) BINARY
                                                   VALUE  3982.
           05 K-KNR-PGV-3983           PIC S9(004) BINARY
                                                   VALUE  3983.
           05 K-KNR-PGV-3984           PIC S9(004) BINARY
                                                   VALUE  3984.
           05 K-KNR-PGV-3985           PIC S9(004) BINARY
                                                   VALUE  3985.
           05 K-KNR-PGV-3987           PIC S9(004) BINARY
                                                   VALUE  3987.
           05 K-KNR-PGV-3988           PIC S9(004) BINARY
                                                   VALUE  3988.
           05 K-KNR-PGV-3989           PIC S9(004) BINARY
                                                   VALUE  3989.
           05 K-KNR-PGV-3990           PIC S9(004) BINARY
                                                   VALUE  3990.
           05 K-KNR-PGV-3992           PIC S9(004) BINARY
                                                   VALUE  3992.
           05 K-KNR-PGV-3997           PIC S9(004) BINARY
                                                   VALUE  3997.
           05 K-KNR-PGV-3998           PIC S9(004) BINARY
                                                   VALUE  3998.
           05 K-KNR-PGV-3999           PIC S9(004) BINARY
                                                   VALUE  3999.
           05 K-KNR-PGV-4000           PIC S9(004) BINARY
                                                   VALUE  4000.
           05 K-KNR-PGV-4001           PIC S9(004) BINARY
                                                   VALUE  4001.
           05 K-KNR-PGV-4002           PIC S9(004) BINARY
                                                   VALUE  4002.
           05 K-KNR-PGV-4003           PIC S9(004) BINARY
                                                   VALUE  4003.
           05 K-KNR-PGV-4004           PIC S9(004) BINARY
                                                   VALUE  4004.
           05 K-KNR-PGV-4005           PIC S9(004) BINARY
                                                   VALUE  4005.
           05 K-KNR-PGV-4006           PIC S9(004) BINARY
                                                   VALUE  4006.
           05 K-KNR-PGV-4007           PIC S9(004) BINARY
                                                   VALUE  4007.
           05 K-KNR-PGV-4008           PIC S9(004) BINARY
                                                   VALUE  4008.
           05 K-KNR-PGV-4009           PIC S9(004) BINARY
                                                   VALUE  4009.
           05 K-KNR-PGV-4010           PIC S9(004) BINARY
                                                   VALUE  4010.
           05 K-KNR-PGV-4011           PIC S9(004) BINARY
                                                   VALUE  4011.
           05 K-KNR-PGV-4012           PIC S9(004) BINARY
                                                   VALUE  4012.
           05 K-KNR-PGV-4013           PIC S9(004) BINARY
                                                   VALUE  4013.
           05 K-KNR-PGV-4015           PIC S9(004) BINARY
                                                   VALUE  4015.
           05 K-KNR-PGV-4016           PIC S9(004) BINARY
                                                   VALUE  4016.
           05 K-KNR-PGV-4018           PIC S9(004) BINARY
                                                   VALUE  4018.
           05 K-KNR-PGV-4020           PIC S9(004) BINARY
                                                   VALUE  4020.
           05 K-KNR-PGV-4022           PIC S9(004) BINARY
                                                   VALUE  4022.
           05 K-KNR-PGV-4026           PIC S9(004) BINARY
                                                   VALUE  4026.
           05 K-KNR-PGV-4030           PIC S9(004) BINARY
                                                   VALUE  4030.
           05 K-KNR-PGV-4033           PIC S9(004) BINARY
                                                   VALUE  4033.
           05 K-KNR-PGV-4034           PIC S9(004) BINARY
                                                   VALUE  4034.
           05 K-KNR-PGV-4035           PIC S9(004) BINARY
                                                   VALUE  4035.
           05 K-KNR-PGV-4040           PIC S9(004) BINARY
                                                   VALUE  4040.
           05 K-KNR-PGV-4044           PIC S9(004) BINARY
                                                   VALUE  4044.
           05 K-KNR-PGV-4045           PIC S9(004) BINARY
                                                   VALUE  4045.
           05 K-KNR-PGV-4052           PIC S9(004) BINARY
                                                   VALUE  4052.
           05 K-KNR-PGV-4055           PIC S9(004) BINARY
                                                   VALUE  4055.
           05 K-KNR-PGV-4060           PIC S9(004) BINARY
                                                   VALUE  4060.
           05 K-KNR-PGV-4061           PIC S9(004) BINARY
                                                   VALUE  4061.
           05 K-KNR-PGV-4062           PIC S9(004) BINARY
                                                   VALUE  4062.
           05 K-KNR-PGV-4064           PIC S9(004) BINARY
                                                   VALUE  4064.
           05 K-KNR-PGV-4065           PIC S9(004) BINARY
                                                   VALUE  4065.
           05 K-KNR-PGV-4073           PIC S9(004) BINARY
                                                   VALUE  4073.
           05 K-KNR-PGV-4080           PIC S9(004) BINARY
                                                   VALUE  4080.
           05 K-KNR-PGV-4085           PIC S9(004) BINARY
                                                   VALUE  4085.
           05 K-KNR-PGV-4086           PIC S9(004) BINARY
                                                   VALUE  4086.
           05 K-KNR-PGV-4087           PIC S9(004) BINARY
                                                   VALUE  4087.
           05 K-KNR-PGV-4096           PIC S9(004) BINARY
                                                   VALUE  4096.
           05 K-KNR-PGV-4099           PIC S9(004) BINARY
                                                   VALUE  4099.
           05 K-KNR-PGV-4119           PIC S9(004) BINARY
                                                   VALUE  4119.
           05 K-KNR-PGV-4199           PIC S9(004) BINARY
                                                   VALUE  4199.
           05 K-KNR-PGV-4900           PIC S9(004) BINARY
                                                   VALUE  4900.
           05 K-KNR-PGV-4902           PIC S9(004) BINARY
                                                   VALUE  4902.
           05 K-KNR-PGV-4903           PIC S9(004) BINARY
                                                   VALUE  4903.
           05 K-KNR-PGV-4905           PIC S9(004) BINARY
                                                   VALUE  4905.
           05 K-KNR-PGV-4907           PIC S9(004) BINARY
                                                   VALUE  4907.
           05 K-KNR-PGV-4908           PIC S9(004) BINARY
                                                   VALUE  4908.
           05 K-KNR-PGV-4912           PIC S9(004) BINARY
                                                   VALUE  4912.
           05 K-KNR-PGV-4913           PIC S9(004) BINARY
                                                   VALUE  4913.
           05 K-KNR-PGV-4919           PIC S9(004) BINARY
                                                   VALUE  4919.
           05 K-KNR-PGV-4920           PIC S9(004) BINARY
                                                   VALUE  4920.
           05 K-KNR-PGV-4921           PIC S9(004) BINARY
                                                   VALUE  4921.
           05 K-KNR-PGV-4922           PIC S9(004) BINARY
                                                   VALUE  4922.
           05 K-KNR-PGV-4927           PIC S9(004) BINARY
                                                   VALUE  4927.
           05 K-KNR-PGV-4925           PIC S9(004) BINARY
                                                   VALUE  4925.
           05 K-KNR-PGV-4935           PIC S9(004) BINARY
                                                   VALUE  4935.
           05 K-KNR-PGV-4942           PIC S9(004) BINARY
                                                   VALUE  4942.
           05 K-KNR-PGV-4943           PIC S9(004) BINARY
                                                   VALUE  4943.
           05 K-KNR-PGV-4944           PIC S9(004) BINARY
                                                   VALUE  4944.
           05 K-KNR-PGV-4945           PIC S9(004) BINARY
                                                   VALUE  4945.
           05 K-KNR-PGV-4946           PIC S9(004) BINARY
                                                   VALUE  4946.
           05 K-KNR-PGV-4958           PIC S9(004) BINARY
                                                   VALUE  4958.
           05 K-KNR-PGV-4980           PIC S9(004) BINARY
                                                   VALUE  4980.
           05 K-KNR-PGV-4982           PIC S9(004) BINARY
                                                   VALUE  4982.
           05 K-KNR-PGV-4983           PIC S9(004) BINARY
                                                   VALUE  4983.
           05 K-KNR-PGV-4984           PIC S9(004) BINARY
                                                   VALUE  4984.
           05 K-KNR-PGV-4985           PIC S9(004) BINARY
                                                   VALUE  4985.
           05 K-KNR-PGV-4987           PIC S9(004) BINARY
                                                   VALUE  4987.
           05 K-KNR-PGV-4988           PIC S9(004) BINARY
                                                   VALUE  4988.
           05 K-KNR-PGV-4989           PIC S9(004) BINARY
                                                   VALUE  4989.
           05 K-KNR-PGV-4990           PIC S9(004) BINARY
                                                   VALUE  4990.
           05 K-KNR-PGV-4992           PIC S9(004) BINARY
                                                   VALUE  4992.
           05 K-KNR-PGV-4094           PIC S9(004) BINARY
                                                   VALUE  4094.
           05 K-KNR-PGV-4097           PIC S9(004) BINARY
                                                   VALUE  4097.
           05 K-KNR-PGV-4997           PIC S9(004) BINARY
                                                   VALUE  4997.
           05 K-KNR-PGV-4998           PIC S9(004) BINARY
                                                   VALUE  4998.
           05 K-KNR-PGV-4999           PIC S9(004) BINARY
                                                   VALUE  4999.
      ************************ ENDE DRAT0146 ************************


