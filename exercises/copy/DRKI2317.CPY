      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2317.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2317.CPv  $
      *
      *   Rev 2.5   Aug 30 2011 12:50:48   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.4   Nov 20 2009 10:57:40   t13ex52
      *AKI00764: SEB-Bank div. Erweiterungen und Aenderungen bei de
      *n JCL's aller Produkte 
      *
      *   Rev 2.3   Jun 20 2009 17:05:16   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.2   Jun 19 2009 17:16:04   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:38:00   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Apr 28 2009 10:13:42   t13ex52
      *Archiverstellung
      *
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                  #####################                         *
      *         ########    D R K I 2 3 1 7    ########                *
      *                  #####################                         *
      *                                                                *
      *   COPY-MEMBER FUER Umsetzung Länder-Kennzeichen von G66-Bank   *
      *           in die Länder-Kennz. der Supporttabelle.             *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.04.09!NEUERSTELLUNG                          !HSL      ! 01  *
      *AKI00764!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *12.11.09 Laenderkennzeichen fuer Daenemark ge-  !HSL      !     *
      *        !aendert von 'DKDNK' auf 'DKDK '        !         !     *
      *--------!---------------------------------------!---------!-----*
      *11.08.11 Gesellschaft neutralisiert             !SWZ      !2.5  *
      *        !                                       !         !     *
      ******************************************************************
      * Felder für das Umsetzen des Länderschlüssels
       01  Z-LAND-SCHL           PIC  X(003).
       01  Z-LAND-SCHL-VALUE.

           05 FILLER             PIC  X(005) VALUE 'ACASC'.
           05 FILLER             PIC  X(005) VALUE 'ADAND'.
           05 FILLER             PIC  X(005) VALUE 'AEARE'.
           05 FILLER             PIC  X(005) VALUE 'AFAFG'.
           05 FILLER             PIC  X(005) VALUE 'AGATG'.
           05 FILLER             PIC  X(005) VALUE 'AIAIA'.
           05 FILLER             PIC  X(005) VALUE 'ALALB'.
           05 FILLER             PIC  X(005) VALUE 'AMARM'.
           05 FILLER             PIC  X(005) VALUE 'ANANT'.
           05 FILLER             PIC  X(005) VALUE 'AOAGO'.
           05 FILLER             PIC  X(005) VALUE 'AQATA'.
           05 FILLER             PIC  X(005) VALUE 'ARARG'.
           05 FILLER             PIC  X(005) VALUE 'ASASM'.
           05 FILLER             PIC  X(005) VALUE 'ATA  '.
           05 FILLER             PIC  X(005) VALUE 'AUAUS'.
           05 FILLER             PIC  X(005) VALUE 'AWABW'.
           05 FILLER             PIC  X(005) VALUE 'AXALA'.
           05 FILLER             PIC  X(005) VALUE 'AZAZE'.
           05 FILLER             PIC  X(005) VALUE 'BABIH'.
           05 FILLER             PIC  X(005) VALUE 'BBBRB'.
           05 FILLER             PIC  X(005) VALUE 'BDBGD'.
           05 FILLER             PIC  X(005) VALUE 'BEB  '.
           05 FILLER             PIC  X(005) VALUE 'BFBFA'.
           05 FILLER             PIC  X(005) VALUE 'BGBGR'.
           05 FILLER             PIC  X(005) VALUE 'BHBHR'.
           05 FILLER             PIC  X(005) VALUE 'BIBDI'.
           05 FILLER             PIC  X(005) VALUE 'BJBEN'.
           05 FILLER             PIC  X(005) VALUE 'BLBLM'.
           05 FILLER             PIC  X(005) VALUE 'BMBMU'.
           05 FILLER             PIC  X(005) VALUE 'BNBRN'.
           05 FILLER             PIC  X(005) VALUE 'BOBOL'.
           05 FILLER             PIC  X(005) VALUE 'BRBRA'.
           05 FILLER             PIC  X(005) VALUE 'BSBHS'.
           05 FILLER             PIC  X(005) VALUE 'BTBTN'.
           05 FILLER             PIC  X(005) VALUE 'BUBUR'.
           05 FILLER             PIC  X(005) VALUE 'BVBVT'.
           05 FILLER             PIC  X(005) VALUE 'BWBWA'.
           05 FILLER             PIC  X(005) VALUE 'BYBLR'.
           05 FILLER             PIC  X(005) VALUE 'BZBLZ'.
           05 FILLER             PIC  X(005) VALUE 'CACAN'.
           05 FILLER             PIC  X(005) VALUE 'CCCCK'.
           05 FILLER             PIC  X(005) VALUE 'CDCOD'.
           05 FILLER             PIC  X(005) VALUE 'CECEE'.
           05 FILLER             PIC  X(005) VALUE 'CFCAF'.
           05 FILLER             PIC  X(005) VALUE 'CGCOG'.
           05 FILLER             PIC  X(005) VALUE 'CHCH '.
           05 FILLER             PIC  X(005) VALUE 'CICIV'.
           05 FILLER             PIC  X(005) VALUE 'CKCOK'.
           05 FILLER             PIC  X(005) VALUE 'CLCHL'.
           05 FILLER             PIC  X(005) VALUE 'CMCMR'.
           05 FILLER             PIC  X(005) VALUE 'CNCHN'.
           05 FILLER             PIC  X(005) VALUE 'COCOL'.
           05 FILLER             PIC  X(005) VALUE 'CPCPT'.
           05 FILLER             PIC  X(005) VALUE 'CRCRI'.
           05 FILLER             PIC  X(005) VALUE 'CSSCG'.
           05 FILLER             PIC  X(005) VALUE 'CSCSK'.
           05 FILLER             PIC  X(005) VALUE 'CUC  '.
           05 FILLER             PIC  X(005) VALUE 'CVCPV'.
           05 FILLER             PIC  X(005) VALUE 'CXCXR'.
           05 FILLER             PIC  X(005) VALUE 'CYCYP'.
           05 FILLER             PIC  X(005) VALUE 'CZCZE'.
           05 FILLER             PIC  X(005) VALUE 'DED  '.
           05 FILLER             PIC  X(005) VALUE 'DGDGA'.
           05 FILLER             PIC  X(005) VALUE 'DJDJI'.
121109     05 FILLER             PIC  X(005) VALUE 'DKDK '.
           05 FILLER             PIC  X(005) VALUE 'DMDMA'.
           05 FILLER             PIC  X(005) VALUE 'DODOM'.
           05 FILLER             PIC  X(005) VALUE 'DZDZA'.
           05 FILLER             PIC  X(005) VALUE 'ECECU'.
           05 FILLER             PIC  X(005) VALUE 'EEEST'.
           05 FILLER             PIC  X(005) VALUE 'EGEGY'.
           05 FILLER             PIC  X(005) VALUE 'EHESH'.
           05 FILLER             PIC  X(005) VALUE 'ERERI'.
           05 FILLER             PIC  X(005) VALUE 'ESE  '.
           05 FILLER             PIC  X(005) VALUE 'ETETH'.
           05 FILLER             PIC  X(005) VALUE 'EU   '.
           05 FILLER             PIC  X(005) VALUE 'FIFIN'.
           05 FILLER             PIC  X(005) VALUE 'FJFJI'.
           05 FILLER             PIC  X(005) VALUE 'FKFLK'.
           05 FILLER             PIC  X(005) VALUE 'FMFSM'.
           05 FILLER             PIC  X(005) VALUE 'FOFRO'.
           05 FILLER             PIC  X(005) VALUE 'FRF  '.
           05 FILLER             PIC  X(005) VALUE 'FXFXX'.
           05 FILLER             PIC  X(005) VALUE 'GAGAB'.
           05 FILLER             PIC  X(005) VALUE 'GBGB '.
           05 FILLER             PIC  X(005) VALUE 'GDGRD'.
           05 FILLER             PIC  X(005) VALUE 'GEGEO'.
           05 FILLER             PIC  X(005) VALUE 'GFGUF'.
           05 FILLER             PIC  X(005) VALUE 'GGGGY'.
           05 FILLER             PIC  X(005) VALUE 'GHGHA'.
           05 FILLER             PIC  X(005) VALUE 'GIGIB'.
           05 FILLER             PIC  X(005) VALUE 'GLGRL'.
           05 FILLER             PIC  X(005) VALUE 'GMGMB'.
           05 FILLER             PIC  X(005) VALUE 'GNGIN'.
           05 FILLER             PIC  X(005) VALUE 'GPGLP'.
           05 FILLER             PIC  X(005) VALUE 'GQGNQ'.
           05 FILLER             PIC  X(005) VALUE 'GRGRC'.
           05 FILLER             PIC  X(005) VALUE 'GSSGS'.
           05 FILLER             PIC  X(005) VALUE 'GTGTM'.
           05 FILLER             PIC  X(005) VALUE 'GUGUM'.
           05 FILLER             PIC  X(005) VALUE 'GWGNB'.
           05 FILLER             PIC  X(005) VALUE 'GYGUY'.
           05 FILLER             PIC  X(005) VALUE 'HKHKG'.
           05 FILLER             PIC  X(005) VALUE 'HMHMD'.
           05 FILLER             PIC  X(005) VALUE 'HNHND'.
           05 FILLER             PIC  X(005) VALUE 'HRHRV'.
           05 FILLER             PIC  X(005) VALUE 'HTHTI'.
           05 FILLER             PIC  X(005) VALUE 'HUH  '.
           05 FILLER             PIC  X(005) VALUE 'IDIDN'.
           05 FILLER             PIC  X(005) VALUE 'IEIRL'.
           05 FILLER             PIC  X(005) VALUE 'ILISR'.
           05 FILLER             PIC  X(005) VALUE 'IMIMN'.
           05 FILLER             PIC  X(005) VALUE 'ININD'.
           05 FILLER             PIC  X(005) VALUE 'IOIOT'.
           05 FILLER             PIC  X(005) VALUE 'IQIRQ'.
           05 FILLER             PIC  X(005) VALUE 'IRIRN'.
           05 FILLER             PIC  X(005) VALUE 'ISISL'.
           05 FILLER             PIC  X(005) VALUE 'ITI  '.
           05 FILLER             PIC  X(005) VALUE 'JEJEY'.
           05 FILLER             PIC  X(005) VALUE 'JMJAM'.
           05 FILLER             PIC  X(005) VALUE 'JOJOR'.
           05 FILLER             PIC  X(005) VALUE 'JPJPN'.
           05 FILLER             PIC  X(005) VALUE 'KEKEN'.
           05 FILLER             PIC  X(005) VALUE 'KGKGZ'.
           05 FILLER             PIC  X(005) VALUE 'KHKHM'.
           05 FILLER             PIC  X(005) VALUE 'KIKIR'.
           05 FILLER             PIC  X(005) VALUE 'KMCOM'.
           05 FILLER             PIC  X(005) VALUE 'KNKNA'.
           05 FILLER             PIC  X(005) VALUE 'KPPRK'.
           05 FILLER             PIC  X(005) VALUE 'KRKOR'.
           05 FILLER             PIC  X(005) VALUE 'KWKWT'.
           05 FILLER             PIC  X(005) VALUE 'KYCYM'.
           05 FILLER             PIC  X(005) VALUE 'KZKAZ'.
           05 FILLER             PIC  X(005) VALUE 'LALAO'.
           05 FILLER             PIC  X(005) VALUE 'LBLBN'.
           05 FILLER             PIC  X(005) VALUE 'LCLCA'.
           05 FILLER             PIC  X(005) VALUE 'LILIE'.
           05 FILLER             PIC  X(005) VALUE 'LKLKA'.
           05 FILLER             PIC  X(005) VALUE 'LRLBR'.
           05 FILLER             PIC  X(005) VALUE 'LSLSO'.
           05 FILLER             PIC  X(005) VALUE 'LTLTU'.
           05 FILLER             PIC  X(005) VALUE 'LULUX'.
           05 FILLER             PIC  X(005) VALUE 'LVLVA'.
           05 FILLER             PIC  X(005) VALUE 'LYLBY'.
           05 FILLER             PIC  X(005) VALUE 'MAMAR'.
           05 FILLER             PIC  X(005) VALUE 'MCMCO'.
           05 FILLER             PIC  X(005) VALUE 'MDMDA'.
           05 FILLER             PIC  X(005) VALUE 'MEMNE'.
           05 FILLER             PIC  X(005) VALUE 'MFMAF'.
           05 FILLER             PIC  X(005) VALUE 'MGMDG'.
           05 FILLER             PIC  X(005) VALUE 'MHMHL'.
           05 FILLER             PIC  X(005) VALUE 'MKMKD'.
           05 FILLER             PIC  X(005) VALUE 'MLMLI'.
           05 FILLER             PIC  X(005) VALUE 'MMMMR'.
           05 FILLER             PIC  X(005) VALUE 'MNMNG'.
           05 FILLER             PIC  X(005) VALUE 'MOMAC'.
           05 FILLER             PIC  X(005) VALUE 'MPMNP'.
           05 FILLER             PIC  X(005) VALUE 'MQMTQ'.
           05 FILLER             PIC  X(005) VALUE 'MRMRT'.
           05 FILLER             PIC  X(005) VALUE 'MSMSR'.
           05 FILLER             PIC  X(005) VALUE 'MTMLT'.
           05 FILLER             PIC  X(005) VALUE 'MUMUS'.
           05 FILLER             PIC  X(005) VALUE 'MVMDV'.
           05 FILLER             PIC  X(005) VALUE 'MWMWI'.
           05 FILLER             PIC  X(005) VALUE 'MXMEX'.
           05 FILLER             PIC  X(005) VALUE 'MYMYS'.
           05 FILLER             PIC  X(005) VALUE 'MZMOZ'.
           05 FILLER             PIC  X(005) VALUE 'NANAM'.
           05 FILLER             PIC  X(005) VALUE 'NCNCL'.
           05 FILLER             PIC  X(005) VALUE 'NENER'.
           05 FILLER             PIC  X(005) VALUE 'NFNFK'.
           05 FILLER             PIC  X(005) VALUE 'NGNGA'.
           05 FILLER             PIC  X(005) VALUE 'NINIC'.
           05 FILLER             PIC  X(005) VALUE 'NLNL '.
           05 FILLER             PIC  X(005) VALUE 'NON  '.
           05 FILLER             PIC  X(005) VALUE 'NPNPL'.
           05 FILLER             PIC  X(005) VALUE 'NRNRU'.
           05 FILLER             PIC  X(005) VALUE 'NTNTZ'.
           05 FILLER             PIC  X(005) VALUE 'NUNIU'.
           05 FILLER             PIC  X(005) VALUE 'NZNZL'.
           05 FILLER             PIC  X(005) VALUE 'OMOMN'.
           05 FILLER             PIC  X(005) VALUE 'PAPAN'.
           05 FILLER             PIC  X(005) VALUE 'PEPER'.
           05 FILLER             PIC  X(005) VALUE 'PFPYF'.
           05 FILLER             PIC  X(005) VALUE 'PGPNG'.
           05 FILLER             PIC  X(005) VALUE 'PHPHL'.
           05 FILLER             PIC  X(005) VALUE 'PKPAK'.
           05 FILLER             PIC  X(005) VALUE 'PLPOL'.
           05 FILLER             PIC  X(005) VALUE 'PMSPM'.
           05 FILLER             PIC  X(005) VALUE 'PNPCN'.
           05 FILLER             PIC  X(005) VALUE 'PRPRI'.
           05 FILLER             PIC  X(005) VALUE 'PSPSE'.
           05 FILLER             PIC  X(005) VALUE 'PTP  '.
           05 FILLER             PIC  X(005) VALUE 'PWPLW'.
           05 FILLER             PIC  X(005) VALUE 'PYPRY'.
           05 FILLER             PIC  X(005) VALUE 'QAQAT'.
           05 FILLER             PIC  X(005) VALUE 'REREU'.
           05 FILLER             PIC  X(005) VALUE 'ROROU'.
           05 FILLER             PIC  X(005) VALUE 'RSSRB'.
           05 FILLER             PIC  X(005) VALUE 'RURUS'.
           05 FILLER             PIC  X(005) VALUE 'RWRWA'.
           05 FILLER             PIC  X(005) VALUE 'SASAU'.
           05 FILLER             PIC  X(005) VALUE 'SBSLB'.
           05 FILLER             PIC  X(005) VALUE 'SCSYC'.
           05 FILLER             PIC  X(005) VALUE 'SDSDN'.
           05 FILLER             PIC  X(005) VALUE 'SES  '.
           05 FILLER             PIC  X(005) VALUE 'SGSGP'.
           05 FILLER             PIC  X(005) VALUE 'SHSHN'.
           05 FILLER             PIC  X(005) VALUE 'SISVN'.
           05 FILLER             PIC  X(005) VALUE 'SJSJM'.
           05 FILLER             PIC  X(005) VALUE 'SKSVK'.
           05 FILLER             PIC  X(005) VALUE 'SLLE '.
           05 FILLER             PIC  X(005) VALUE 'SMSMR'.
           05 FILLER             PIC  X(005) VALUE 'SNSEN'.
           05 FILLER             PIC  X(005) VALUE 'SOSOM'.
           05 FILLER             PIC  X(005) VALUE 'SRSUR'.
           05 FILLER             PIC  X(005) VALUE 'STSTP'.
           05 FILLER             PIC  X(005) VALUE 'SUSUN'.
           05 FILLER             PIC  X(005) VALUE 'SVSLV'.
           05 FILLER             PIC  X(005) VALUE 'SYSYR'.
           05 FILLER             PIC  X(005) VALUE 'SZSWZ'.
           05 FILLER             PIC  X(005) VALUE 'TATAA'.
           05 FILLER             PIC  X(005) VALUE 'TCTCA'.
           05 FILLER             PIC  X(005) VALUE 'TDTCD'.
           05 FILLER             PIC  X(005) VALUE 'TFATF'.
           05 FILLER             PIC  X(005) VALUE 'TGTGO'.
           05 FILLER             PIC  X(005) VALUE 'THTHA'.
           05 FILLER             PIC  X(005) VALUE 'TJTJK'.
           05 FILLER             PIC  X(005) VALUE 'TKTKL'.
           05 FILLER             PIC  X(005) VALUE 'TLTLS'.
           05 FILLER             PIC  X(005) VALUE 'TMTKM'.
           05 FILLER             PIC  X(005) VALUE 'TNTUN'.
           05 FILLER             PIC  X(005) VALUE 'TOTON'.
           05 FILLER             PIC  X(005) VALUE 'TRTUR'.
           05 FILLER             PIC  X(005) VALUE 'TTTTO'.
           05 FILLER             PIC  X(005) VALUE 'TVTUV'.
           05 FILLER             PIC  X(005) VALUE 'TWTWN'.
           05 FILLER             PIC  X(005) VALUE 'TZTZA'.
           05 FILLER             PIC  X(005) VALUE 'UAUKR'.
           05 FILLER             PIC  X(005) VALUE 'UGUGA'.
           05 FILLER             PIC  X(005) VALUE 'UMUMI'.
           05 FILLER             PIC  X(005) VALUE 'USUSA'.
           05 FILLER             PIC  X(005) VALUE 'UYURY'.
           05 FILLER             PIC  X(005) VALUE 'UZUZB'.
           05 FILLER             PIC  X(005) VALUE 'VAVAT'.
           05 FILLER             PIC  X(005) VALUE 'VCVCT'.
           05 FILLER             PIC  X(005) VALUE 'VEVEN'.
           05 FILLER             PIC  X(005) VALUE 'VGVGB'.
           05 FILLER             PIC  X(005) VALUE 'VIVIR'.
           05 FILLER             PIC  X(005) VALUE 'VNVNM'.
           05 FILLER             PIC  X(005) VALUE 'VUVUT'.
           05 FILLER             PIC  X(005) VALUE 'WFWLF'.
           05 FILLER             PIC  X(005) VALUE 'WSWSM'.
           05 FILLER             PIC  X(005) VALUE 'YEYEM'.
           05 FILLER             PIC  X(005) VALUE 'YTMYT'.
           05 FILLER             PIC  X(005) VALUE 'YUYUG'.
           05 FILLER             PIC  X(005) VALUE 'ZAZAF'.
           05 FILLER             PIC  X(005) VALUE 'ZMZMB'.
           05 FILLER             PIC  X(005) VALUE 'ZRZAR'.
           05 FILLER             PIC  X(005) VALUE 'ZWZWE'.

       01  Z-LAND-SCHL-ARR REDEFINES Z-LAND-SCHL-VALUE.
           05 Z-LAND-SCHL-OCC   OCCURS 260.
110811        10  Z-LAND-SCHL-G66   PIC  X(002).
              10  Z-LAND-SCHL-KI    PIC  X(003).
       01  Z-LAND-SCHL-MAX          PIC S9(009) VALUE 260.

      * INDEX FüR DAS UMSETZEN DES LÄNDERSCHLUESSELS
       01  I-LAND-SCHL              PIC  9(004) VALUE 0.

      ******************************************************************
      ************************** ENDE DRKI2317 *************************
      ******************************************************************
