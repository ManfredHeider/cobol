      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM802S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM802S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:36:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    13.04.92 22:22:54
      *=================================================================
      ******************************************************************
      *
      *    I N F O P O O L - D E F I N I T I O N E N        06.03.87
      *
      *    FUER DIE PROGRAMME INFOLOAD UND INFOMAN (WORKING-STORAGE)
      *                       UM805        UM803 (ONLINE)
      *                                    UM806020 (Batch)
      *
      *    ==> : ZEILENINHALT IST ABHAENGIG VON DEN USERVORGABEN FUER
      *    DIE LAENGE EINER ALLOCATION-UNIT BZW. EINES DIRECTORY-ENTRIES
      *
      *-----------------------------------------------------------------
      *    I - O - A R E A S
      *-----------------------------------------------------------------
       01  IP-IOAREA.
           02  AUNUM          PIC   9(0008) COMP.
           02  AU.
==>          05  AUCHAR       PIC   X(0001) OCCURS 1860 TIMES.
           02  FILLER         REDEFINES  AU.
      *          W-DE-OCCURS-ANGABE = AUDES (ANZAHL DE PRO AU)
==>          05  W-DE         PIC   X(0186) OCCURS 10 TIMES.
       01  IP-IOAREA-2.
           02  AUNUM-2        PIC  S9(0008) COMP.
==>        02  AU-2           PIC   X(1860).
      *-----------------------------------------------------------------
      *    A U   M I T   V E R W A L T U N G S S A T Z
      *-----------------------------------------------------------------
       01  VWS.
           02  IP-KONSTANTEN.
      *
      *    I N F O P O O L - C H A R A C T E R I S T I K
      *
      *    AUANZAHL ANZAHL AU (ALLOCATION UNIT) GESAMT: > 3
      *    DEANZAHL ANZAHL DE (DIRECTORY ENTRY) : <100000
      *             UND = PRIMZAHL * AUDES
      *    AULAENGE AU-LAENGE IN BYTES (MINIMUM 186, MAXIMUM 32000)
      *    DELAENGE DE-LAENGE IN BYTES : <= AULAENGE, MINIMUM 186
      *    AUDES    ANZAHL DE IN EINEM AU = AULAENGE / DELAENGE
      *    DEADR    ANZAHL ADRESSEN IM DE MAXIMAL = (DELAENGE - 32) / 4
      *    IDAUS    ANZAHL (PRIMZAHL) AU FUER DIREC.= DEANZAHL / AUDES
      *    IDFIRST  1. AU-NUMMER FUER INFOPOOL-DIRECTORY (ID) = 2
      *    IDLAST   LETZTE AU-NUMMER FUER ID = IDAUS + 1
      *    ATAUS    ANZAHL AT-AU = (AUANZAHL - 1) / (AULAENGE * 8) + 1
      *    DPFIRST  NUMMER VOM 1. DP-AU (DATAPOOL) = ATAUS + IDAUS + 2
      *
      *    !!! DIE VALUE-ANGABEN FUER AULAENGE UND DELAENGE SIND
      *    !!! ERFORDERLICH. DIE DAVON ABGELEITETEN WERTE FUER
      *    !!! AUDES UND DEADR UND IDFIRST ALS VALUE-ANGABEN ERSPAREN
      *    !!! IM PROGRAMM INFOMAN ZUGRIFFE AUF DEN VERWALTUNGSSATZ.
      *
            05 AUANZAHL       PIC  S9(0008) COMP   VALUE       +0.
            05 DEANZAHL       PIC  S9(0008) COMP   VALUE       +0.
==>         05 AULAENGE       PIC  S9(0004) COMP   VALUE    +1860.
==>         05 DELAENGE       PIC  S9(0004) COMP   VALUE     +186.
==>         05 AUDES          PIC  S9(0004) COMP   VALUE      +10.
==>         05 DEADR          PIC  S9(0004) COMP   VALUE      +38.
            05 IDAUS          PIC  S9(0008) COMP   VALUE       +0.
==>         05 IDFIRST        PIC  S9(0008) COMP   VALUE       +2.
            05 IDLAST         PIC  S9(0008) COMP   VALUE       +0.
            05 ATAUS          PIC  S9(0008) COMP   VALUE       +0.
            05 DPFIRST        PIC  S9(0008) COMP   VALUE       +0.
           02 IP-VARIABLEN.
            05 AUBELEGT       PIC  S9(0008) COMP   VALUE       +0.
            05 DEBELEGT-V02   PIC  S9(0008) COMP   VALUE   +99999.
            05 DELASTNR       PIC  S9(0008) COMP   VALUE       +0.
            05 SYSSERVICE-ID  PIC   X(0008)        VALUE   SPACES.
            05 VERSION-X.
               10  FILLER     PIC   X(0004)          VALUE   '0300'.
            05  FILLER        REDEFINES VERSION-X.
               10  IP-VERSION PIC   X(0002).
               10  FILLER     PIC   X(0002).
            05 ATLASTUP       PIC  S9(0008) COMP   VALUE       +0.
            05 DEBELEGT       PIC  S9(0008) COMP   VALUE       +0.
            05 HASH-KEY-LENGTH PIC S9(0004) COMP   VALUE      +20.
            05 RECHNER        PIC   X(0008)        VALUE   SPACES.
      *        VWS-REST-LAENGE = AULAENGE - 78.
==>         05 VWS-REST       PIC  X(1782)         VALUE   SPACES.
      *-----------------------------------------------------------------
      *    D I R E C T O R Y - E N T R I E S
      *-----------------------------------------------------------------
       01  DE-S.
           03  DE-BER                       OCCURS 2 TIMES.
           04  DE-FOLGE-DE                   PIC  9(0005).
           04  DE-DEIDENT.
            05  DE-LIB                       PIC  X(0001).
              88  DE-INIT                         VALUE LOW-VALUE.
              88  DE-FREE                         VALUE SPACE.
              88  DE-PRIVATE                      VALUE '1'.
              88  DE-PUBLIC                       VALUE '2'.
            05  DE-PUBLIC-ID.
              10  DE-PUBLIC-ID-ST            PIC  X(0001)   OCCURS 8.
            05  DE-DENAME.
             10  DE-NAME                     PIC  X(0008).
             10  DE-SUFFIX                   PIC  X(0003).
             10  DE-KEY-ERG-LAENGE           PIC  9(0002).
             10  DE-LFDNR                    PIC  9(0004).
           04  FILLER                        PIC  X(0001).
           04  DE-ADRAUBER-FOLGE-DE-LANG.
            05  DE-ADRAUBER-LANG.
               15  DE-KEY-ERG                PIC  X(0064).
      *            DE-ADRAUBER-KURZ-LAENGE = (DEADR - 32) * 4
==>            15  DE-ADRAUBER-KURZ          PIC  X(0024).
            05  FILLER                       PIC  X(0004).
            05  DE-MANAGEMENT-FELDER.
             06  DE-OWNERID                  PIC  X(0008).
             06  DE-CODE                     PIC  X(0001).
             06  DE-FORMAT                   PIC  X(0001).
             06  DE-STATUS                   PIC  X(0001).
             06  DE-LOGONZAEHLER             PIC  9(0005).
             06  DE-FILELAENGE               PIC  9(0008).
             06  DE-ALLOCLAENGE              PIC  9(0008).
             06  DE-LAST-UPDATE.
              07  DE-DATUM-LAST-UPDATE.
               10  DE-JAHR-LU                PIC  9(0002).
               10  DE-MONAT-LU               PIC  9(0002).
               10  DE-TAG-LU                 PIC  9(0002).
              07  DE-ZEIT-LU.
               10  DE-STUNDE-LU              PIC  9(0002).
               10  DE-MINUTE-LU              PIC  9(0002).
               10  DE-SECUNDE-LU             PIC  9(0002).
               10  DE-HSEC-LU                PIC  9(0002).
             06  DE-LAST-READ.
              07  DE-DATUM-LAST-READ.
               10  DE-JAHR-LR                PIC  9(0002).
               10  DE-MONAT-LR               PIC  9(0002).
               10  DE-TAG-LR                 PIC  9(0002).
              07  DE-ZEIT-LR.
               10  DE-STUNDE-LR              PIC  9(0002).
               10  DE-MINUTE-LR              PIC  9(0002).
               10  DE-SECUNDE-LR             PIC  9(0002).
               10  DE-HSEC-LR                PIC  9(0002).
           04  FILLER    REDEFINES  DE-ADRAUBER-FOLGE-DE-LANG.
            05  FILLER                       PIC  X(0064).
      *         DE-ADRAUBER-FOLGE-DE-KURZ-LAENGE = (DEADR - 16) * 4
==>         05  DE-ADRAUBER-FOLGE-DE-KURZ    PIC  X(0088).
      *        FILLER-LAENGE = DELAENGE - 32 - DEADR * 4
==>        04  FILLER                        PIC  X(0002).
      *-----------------------------------------------------------------
      *    HILFSBEREICH ZUR EINHEITLICHEN BEHANDLUNG DER ADRESSLEISTEN
      *-----------------------------------------------------------------
       01  DE-ADRAU-HILFSBER.
           04  DE-ADRAU-FOLGE-DE-LANG.
      *         DE-ADRAU-OCCURS-ANGABE = DEADR
==>         05  DE-ADRAU OCCURS  38 TIMES    PIC S9(0008) COMP.
       01  FILLER      REDEFINES DE-ADRAU-HILFSBER.
           04  DE-ADRAU-LANG.
            05  DE-ADRAU-FOLGE-DE-KURZ.
==>          10  DE-ADRAU-KURZ               PIC  X(0024).
             10  FILLER                      PIC  X(0064).
           04  FILLER                        PIC  X(0064).
      *-----------------------------------------------------------------
      *    A U   F U E R   A L L O C A T I O N - T A B L E
      *-----------------------------------------------------------------
       01  AUTAB.
==>        02 ATAB            PIC  X(0001)   OCCURS 1860 TIMES.
