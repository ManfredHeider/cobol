      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0111.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0111.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.4   09 Feb 1999 11:01:12   T13EX12
      *AKI00014 - Tabelle Adressaenderung aufgenommen
      *
      *   Rev 1.3   14 May 1998 16:22:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.2   14 May 1998 14:31:06   P13SP99
      *Neuerstellung
      *
      *   Rev 1.1   14 May 1998 10:33:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0111 - KONSTANTEN FÜR PERSONENROLLEN      *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMITZ                                          *
      *    DATUM:    10.02.1998                                       *
      *    STAND:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!10.02.98! 01 ! NEU                       ! SCHMITZ  *
      *         !22.04.98! 02 ! AKTUALISIERUNG ANALOG DOKU! HUEBNER  *
      *         !23.04.98! 03 ! AKTUALISIERUNG ANALOG DOKU! HUEBNER  *
      * -------------------------------------------------------------*
      *         !24.10.98! 04 ! Tabelle Adressaenderung   !v.d.Goltz *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      * NAMENSZUSAMMENSETZUNG:
      *
      * K-PERSROLLE-BEITRZAHLER
      * !     !          !
      * !     !          +------- AUSPRAEGUNG
      * !     +------------------ PERSONENROLLE
      * +------------------------ K = KONSTANTE
      *
      *
      *    AUSPRÄGUNGEN DER TABELLE PERSONENROLLE
      *
       01  PERSONENROLLE.
           05  K-PERSROLLE-BEITRZAHLER   PIC X(001) VALUE 'Z'.
           05  K-PERSROLLE-BZUGSBRCHTGTR PIC X(001) VALUE 'B'.
           05  K-PERSROLLE-IDMA          PIC X(001) VALUE 'I'.
           05  K-PERSROLLE-KDINT         PIC X(001) VALUE 'K'.
           05  K-PERSROLLE-VB            PIC X(001) VALUE 'V'.
           05  K-PERSROLLE-VPERS         PIC X(001) VALUE 'P'.
           05  K-PERSROLLE-VN            PIC X(001) VALUE 'N'.
220498     05  K-PERSROLLE-ADMA          PIC X(001) VALUE 'A'.

      *
      *    VERWENDUNG DER PERSONENROLLE
      *
      * K-PR-KDINT-AQUDATEN
      * ! !   !          !
      * ! !   !          +------- AUSPRAEGUNG OHNE SUFFIX
      * ! !   +------------------ PERSONENROLLENAUSPRAEGUNG
      * ! +---------------------- PERSONENROLLE
      * +------------------------ K = KONSTANTE
      *
      * IM FALLE EINER DOPPELBEZEICHNUNG WURDE DER BEZUG ZUR
      * ENTSPR. TABELLE ZUSAETZLICH ANGEHANGEN.
      *
      *
      *
       01  TABELLE-AKQUISEDATEN.
           05  K-PR-KDINT-AQUDATEN       PIC X(001) VALUE 'K'.
      *
       01  TABELLE-ARBAUFTRAGEMPFBEZ.
           05  K-PR-VB-EMPFAENGER        PIC X(001) VALUE 'V'.
      *
       01  TABELLE-ARBEITSAUFTRAG.
           05  K-PR-VB-HYP               PIC X(001) VALUE 'V'.
      *
       01  TABELLE-ARBEITSSTATION.
           05  K-PR-VB-SPVBROLLENKZ      PIC X(001) VALUE 'V'.
      *
       01  TABELLE-AUSSENDIENSTMA.
           05 K-PR-VB-ADMA-ADM           PIC X(001) VALUE 'V'.
      *
       01  TABELLE-AUTBENUTZERBEZ.
           05 K-PR-VB-ABBPERS            PIC X(001) VALUE 'V'.
           05 K-PR-IDMA-ABBPERS          PIC X(001) VALUE 'I'.
      *
       01  TABELLE-BENUTZER.
           05 K-PR-VB-BENMITAR           PIC X(001) VALUE 'V'.
           05 K-PR-IDMA-BENMITAR         PIC X(001) VALUE 'I'.
      *
       01  TABELLE-BENUTZER-SETTINGS.
           05 K-PR-VB-BESBENUTZER        PIC X(001) VALUE 'V'.
           05 K-PR-IDMA-BESBENUTZER      PIC X(001) VALUE 'I'.
      *
       01  TABELLE-BESTELLUNG.
           05 K-PR-VB-BESTELLER-BEST     PIC X(001) VALUE 'V'.
      *
       01  TABELLE-DATENFELDDEF.
           05 K-PR-VB-DATFDEFVB          PIC X(001) VALUE 'V'.
      *
       01  TABELLE-DATENFELDWERT.
           05 K-PR-VB-DATFWERTVB         PIC X(001) VALUE 'V'.
      *
       01  TABELLE-DFUEVERBINDUNG.
           05 K-PR-VB-VB-DFUE            PIC X(001) VALUE 'V'.
           05 K-PR-VB-PCBESITZER-DFUE    PIC X(001) VALUE 'V'.
      *
       01  TABELLE-DOKUMENT.
           05 K-PR-VB-ERSTELLER          PIC X(001) VALUE 'V'.
      *
       01  TABELLE-EINKOMMEN.
           05 K-PR-KDINT-EINKOPERS       PIC X(001) VALUE 'K'.
      *
       01  TABELLE-EXTSERVBEZ.
           05 K-PR-VB-STATADMA-ESB       PIC X(001) VALUE 'V'.
      *
       01  TABELLE-FENSTER-VORGABEN.
           05 K-PR-VB-FEVBENUTZER        PIC X(001) VALUE 'V'.
      *
       01  TABELLE-GRUPPENBENUTZERBEZ.
           05 K-PR-VB-GBBMITA            PIC X(001) VALUE 'V'.
           05 K-PR-IDMA-GBBMITA          PIC X(001) VALUE 'I'.
      *
       01  TABELLE-HAUSWOHNEIGENTUM.
           05 K-PR-KDINT-HWEPERS         PIC X(001) VALUE 'K'.
      *
       01  TABELLE-HYPOTHEK.
           05 K-PR-KDINT-HYPPERS         PIC X(001) VALUE 'K'.
      *
       01  TABELLE-INTERNERSERVICEBEZ.
           05 K-PR-VB-STATADMA-ISB       PIC X(001) VALUE 'V'.
      *
       01  TABELLE-KAUFBAUWUNSCH.
           05 K-PR-KDINT-KBWPERS         PIC X(001) VALUE 'K'.
      *
       01  TABELLE-KISTATISTIK.
           05 K-PR-KDINT-KUND            PIC X(001) VALUE 'K'.
           05 K-PR-VB-PCBESITZER-KISTAT  PIC X(001) VALUE 'V'.
           05 K-PR-VB-VB-KISTAT          PIC X(001) VALUE 'V'.
      *
       01  TABELLE-KUNDENBESTELLUNG.
           05 K-PR-VB-BESTELLER-KB       PIC X(001) VALUE 'V'.
      *
       01  TABELLE-KUNDENINTERESSENT.
           05 K-PR-KDINT-KIPERS          PIC X(001) VALUE 'K'.
           05 K-PR-VB-KIVMPERS           PIC X(001) VALUE 'V'.
      *
       01  TABELLE-KUNDENINTVBBEZ.
           05 K-PR-VB-KVBVBPERS          PIC X(001) VALUE 'V'.
           05 K-PR-KDINT-KVBKIPERS       PIC X(001) VALUE 'K'.
      *
       01  TABELLE-KUNDEVBMAGAZIN.
           05 K-PR-KDINT-KUNDEINT        PIC X(001) VALUE 'K'.
           05 K-PR-VB-MITARB             PIC X(001) VALUE 'V'.
      *
       01  TABELLE-LERNDATEN.
           05 K-PR-VB-STATADMA-LD        PIC X(001) VALUE 'V'.
      *
       01  TABELLE-MIETE.
           05 K-PR-KDINT-MIETEPERS       PIC X(001) VALUE 'K'.
      *
       01  TABELLE-MITARBEITER.
           05 K-PR-VB-MITARBPERS         PIC X(001) VALUE 'V'.
           05 K-PR-IDMA-MAPERS           PIC X(001) VALUE 'I'.
           05 K-PR-VB-MITARBVORGES       PIC X(001) VALUE 'V'.
      *
       01  TABELLE-PROZESS-VORGABEN.
           05 K-PR-VB-PRVBENUTZER        PIC X(001) VALUE 'V'.
           05 K-PR-KDINT-PRVBENUTZER     PIC X(001) VALUE 'I'.
      *
       01  TABELLE-SPARBETRAG.
           05 K-PR-KDINT-SPBETRPERS      PIC X(001) VALUE 'K'.
      *
       01  TABELLE-STATISTIK.
           05 K-PR-VB-ADMA-ST            PIC X(001) VALUE 'V'.
      *
       01  TABELLE-VERSTARIFROLLE.
           05 K-PR-VPERS-VTRPERS         PIC X(001) VALUE 'P'.
      *
       01  TABELLE-VERTRAGROLLEBEZ.
           05 K-PR-VB-VRBPERS            PIC X(001) VALUE 'V'.
           05 K-PR-VN-VRBPERS            PIC X(001) VALUE 'N'.
           05 K-PR-BEITRZAHLER-VRBPERS   PIC X(001) VALUE 'Z'.
      *
       01  TABELLE-ZIELEWUENSCHE.
           05 K-PR-KDINT-ZWPERS          PIC X(001) VALUE 'K'.

220498 01  TABELLE-ZUKUENFTEINNAHMEN.
220498     05 K-PR-KDINT-ZEPERS          PIC X(001) VALUE 'K'.

241098 01  TABELLE-ADRESSAENDERUNG.
241098     05 K-PR-VB-NT-ADAVB           PIC X(001) VALUE 'V'.

      ******** ENDE COPY-MEMBER KONSTANTEN (DRKI0111)      ********
