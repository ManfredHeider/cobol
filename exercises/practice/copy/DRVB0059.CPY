      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0059.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRVB0059.CPv  $
      *
      *   Rev 2.8   Aug 29 2016 13:16:22   t13ex57
      *AAS01183: Schnittstellenerweiterung 34i
      *
      *   Rev 2.7   Aug 24 2016 13:32:04   t13ex57
      *AAS01183: Schnittstellenerweiterung 34i
      *
      *   Rev 2.6   Aug 24 2016 09:04:56   t13ex57
      *AAS01183: Schnittstellenerweiterung 34i
      *
      *   Rev 2.5   Mar 07 2013 10:14:06   t13ex57
      *AAS01057: Erweiterung SST zum VB-Shop
      *
      *   Rev 2.4   Mar 02 2009 14:07:40   t13ex57
      *AAS00806: HOST-Schnittstellenerweiterung
      *
      *   Rev 2.3   Mar 13 2008 12:12:26   t13ex57
      *AKD00135: Erweiterung HOST-Schnittstelle VB-Shop NEU
      *
      *   Rev 2.2   17 Jan 2008 09:44:26   t13ex57
      *AAS00721: Warenwirtschaft Schnittstellenerweiterung
      *
      *   Rev 2.1   Mar 11 2002 12:15:30   t13ae28
      *AVB00556: Ramsis-SST wg. System4 angepasst
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Aug 13 1996 16:39:32   T13AE26
      *AFVB0290: Umsetzung P.-Stufe MS-Nr eingefügt
      *
      *   Rev 1.0   Nov 04 1995 09:14:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0059                                      *
      *    --------------------------------------------------------   *
      *    SATZBESCHREIBUNG DATEI ALV VB-STAMMDATEN                   *
      *    FUER EINSTELLUNG IN DEN INFOPOOL                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERGHOFER                                        *
      *    DATUM:    27.06.1994                                       *
      *****************************************************************
      *    VERSION   1    05.08.94 16:13:17
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0123!27.06.94! 01 ! NEU ERSTELLT              ! BERGHOFER*
      * AFVB0290!25.06.96!    ! Kommentar eingefügt       !          *
      *         !        ! 02 ! + Spalten >72 entf.       ! T. Moll  *
      *         !        !    !                           !          *
      * AVB00556!27.09.01! 06 ! ENTFERNEN DOPPELTE BANK-  ! D.EHRMANN*
      *         !        !    ! SÄTZE UND ENTFERNEN CR+LF !          *
      *         !        !    ! IM BANK-SATZ              !          *
      * AAS00721!02.01.08! 07 ! ERWEITERUNG UM EMAIL; HOME! U. BOLL  *
      *         !        !    ! PAGE UND VERMITTLERNUMMER !          *
100308* AKD00135!10.03.08! 08 ! WWS SST ERWEITERUNG       ! U. BOLL  *
181108* AAS00795!18.11.08! 09 ! neue Spalten f. email.dat ! U.Boll   *
270109* AAS00806!27.01.09!2.10! Erweiterung um Praxis/akt.! U. Boll  *
      *         !        !    ! Strukturstufe             !          *
170113* AAS01057!17.01.13!2.11! Erweiterung 34F Daten     ! U. Boll  *
290816* AAS01183!29.08.16!2.12! Erweiterung 34I Daten     ! U. Boll  *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * Vorsicht: MS_NR in der Datei 'STAMM' enthält nicht immer     *
      * --------  die aktuelle Stufennummer !                        *
      *           Bei VB's mit Praxisstufe wird hier ev. die höhere  *
      *           dem Provisionssatz entsprechende Stufe geliefert.  *
      *           (vergl. Prog.doku; AFVB0290)                       *
      ****************************************************************
      *    DRVB0059-ST-NR               VB-NUMMER                    *
      *    DRVB0059-ST-PRUEFZIFFER      VB-NUMMER/PRÜFZIFFER         *
      *    DRVB0059-KNR-GESELL-TYP      GESELLSCHFTSTYP (ALV=BRANCHE)*
      *    DRVB0059-KNR-GESELL-TEXT     GESELLSCHAFTSTYP KURZTEXT    *
      *                                                              *
 ***  * ++ DRVB0059-MS-NR               STUFENNUMMER                 *
 ***  * ++ Zu MS-NR: Kommentar zu AFVB0290 beachten !!               *
      *                                                              *
      *    DRVB0059-MS-BEZ-KURZ         STUFE KURZTEXT               *
      *    DRVB0059-BS-NR               BONUS-STUFE                  *
      *    DRVB0059-MSBZ-DAT-VON        AB WANN DIE STUFE ERREICHT...*
      *    DRVB0059-DIR-NR              DIREKTIONSNUMMER             *
      *    DRVB0059-MAT-DAT-BIS         AUSTRITTSDATUM               *
      *    DRVB0059-MWST-PFLICHT        MWST-PFLICHTIG J ODER N      *
      *    DRVB0059-RECHN-ART           ART DER BEZAHLUNG(L=LASTSCHRIFT)
      *    DRVB0059-PILOT-TEXT          PILOTENKOFFER TEXTZEILE      *
      *    DRVB0059-PILOT-DAT-VON       PILOTENKOFFER SEIT           *
      *    DRVB0059-ATTACHE-TEXT        ATTACHE-KOFFER TEXTZEILE     *
      *    DRVB0059-ATTACHE-DAT-VON     ATTACHE-KOFFER SEIT          *
      *    DRVB0059-KLEBEBINDE-TEXT     KLEBEBINDEGERAET TEXTZEILE   *
      *    DRVB0059-KLEBEBINDE-DAT-VON  KLEBEBINDEGERAET SEIT        *
      *    DRVB0059-HANDB-DVAG-TEXT     DVBG-HANDBUCH TEXTZEILE      *
      *    DRVB0059-HANDB-DVAG-DAT-VON  DVBG-HANDBUCH SEIT           *
      *    DRVB0059-HANDB-AVBG-TEXT     AVBG-HANDBUCH TEXTZEILE      *
      *    DRVB0059-HANDB-AVBG-DAT-VON  AVBG-HANDBUCH SEIT           *
020108*    DRVB0059-REGIST-NR           VERMITTLERNUMMER             *
020108*    DRVB0059-EMAIL-ADR           EMAILADRESSE                 *
020108*    DRVB0059-HOMEPAGE            VB-HOMEPAGE                  *
      *                                                              *
      ****************************************************************
       01  DRVB0059-SATZ.
           05  DRVB0059-ST-NR                  PIC 9(007).
           05  DRVB0059-ST-PRUEFZIFFER         PIC X(001).
           05  DRVB0059-KNR-GESELL-TYP         PIC 9(002).
           05  DRVB0059-KNR-GESELL-TEXT        PIC X(005).
           05  DRVB0059-MS-NR                  PIC 9(003).
 ***  * ++ Zu MS-NR: Kommentar zu AFVB0290 beachten !!               *
           05  DRVB0059-MS-BEZ-KURZ            PIC X(004).
           05  DRVB0059-BS-NR                  PIC 9(003).
           05  DRVB0059-MSBZ-DAT-VON           PIC X(010).
           05  DRVB0059-DIR-NR                 PIC 9(003).
           05  DRVB0059-MAT-DAT-BIS            PIC X(010).
           05  DRVB0059-MWST-PFLICHT           PIC X(001).
           05  DRVB0059-RECHN-ART              PIC X(001).
           05  DRVB0059-PILOT-TEXT             PIC X(010).
           05  DRVB0059-PILOT-DAT-VON          PIC X(010).
           05  DRVB0059-ATTACHE-TEXT           PIC X(010).
           05  DRVB0059-ATTACHE-DAT-VON        PIC X(010).
           05  DRVB0059-KLEBEBINDE-TEXT        PIC X(010).
           05  DRVB0059-KLEBEBINDE-DAT-VON     PIC X(010).
           05  DRVB0059-HANDB-DVAG-TEXT        PIC X(010).
           05  DRVB0059-HANDB-DVAG-DAT-VON     PIC X(010).
           05  DRVB0059-HANDB-AVBG-TEXT        PIC X(010).
           05  DRVB0059-HANDB-AVBG-DAT-VON     PIC X(010).
020108     05  DRVB0059-REGIST-NR              PIC X(030).
020108     05  DRVB0059-EMAIL-ADR              PIC X(083).
020108     05  DRVB0059-HOMEPAGE               PIC X(083).
100308     05  DRVB0059-HRD-ORT                PIC X(40).
100308     05  DRVB0059-HRD-EINTR              PIC X(008).
100308     05  DRVB0059-HROE-EINTR-VA          PIC X(25).
100308     05  DRVB0059-HROE-EINTR-VB          PIC X(25).
100308     05  DRVB0059-KNR-MA-TYP             PIC 9(001).
100308     05  DRVB0059-MA-BDV-MITGLIED        PIC X(1).
181108     05  DRVB0059-STEUERNR               PIC X(015).
181108     05  DRVB0059-BESTELLFLAG            PIC X.
270109     05  DRVB0059-STUFE-AKTUELL          PIC 9(003).
270109     05  DRVB0059-PRAXIS                 PIC 9(001).
170113     05  DRVB0059-PRUEFUNG-34F-JN        PIC X(1).
170113     05  DRVB0059-34F-AMT                PIC X(060).
170113     05  DRVB0059-34F-STRASSE            PIC X(060).
170113     05  DRVB0059-MA-34F-ORT             PIC X(060).
170113     05  DRVB0059-34F-PLZ                PIC X(020).
170113     05  DRVB0059-MA-34F-REG-NR          PIC X(015).
280716     05  DRVB0059-PRUEFUNG-34I-JN        PIC X(1).
280716     05  DRVB0059-34I-AMT                PIC X(060).
280716     05  DRVB0059-34I-STRASSE            PIC X(060).
280716     05  DRVB0059-MA-34I-ORT             PIC X(060).
280716     05  DRVB0059-34I-PLZ                PIC X(020).
280716     05  DRVB0059-MA-34I-REG-NR          PIC X(015).
270901*    05  DRVB0059-ZEILENENDE.
270901*        10  DRVB0059-CR                 PIC X(001).
270901*        10  DRVB0059-LF                 PIC X(001).
      *
      ******** ENDE COPY-MEMBER (DRVB0059) *******************


