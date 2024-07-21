      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0411.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA0411.CPv  $
      *
      *   Rev 2.9   Mar 17 2020 08:34:38   T13ae37
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - PVW
      *
      *   Rev 2.8   May 14 2018 11:11:56   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.7   Mar 28 2017 08:13:58   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - PVW
      *
      *   Rev 2.6   Oct 02 2015 09:14:18   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.5   May 04 2015 11:07:42   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.4   Jan 10 2014 12:31:36   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.3   Jun 19 2012 09:23:50   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:48:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 09:32:14   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:29:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:55:16   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:33:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0411                                      *
      *    --------------------------------------------------------   *
      *    PMBE-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00622!01.06.07!2.01! NEUES FELD KNR-VGES       ! STORCH   *
      * APA01101!23.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!08.04.13! 2.2! ERGÄNZUNGEN FÜR KALYPSO   ! BÖTTCHER *
      * APA01932!31.03.15! 2.3! ERGÄNZUNGEN FÜR LUCY      ! KÖPRÜCÜ  *
      *         !02.10.15! 2.4! NEUES FELD VP-NR (CKV)    ! KÖPRÜCÜ  *
      * APA02353!21.02.17! 2.7! TELESON                   ! BUCH     *
      * APA02579!24.04.18! 2.8! GENERALI LEBEN            ! SCHEIDEL *
      * APA02884!18.11.19! 2.9! PMBE-BEZUGSJAHR           ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PMBE.                   *
      *                                                              *
      ****************************************************************
      *
             30 PMBE-DATEN.
                35 PMBE-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PMBE-VERSION        PIC S9(004)       BINARY.
                35 KNR-PMBE-ART        PIC S9(004)       BINARY.
                35 PDTA-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PRINTE              PIC S9(015)       PACKED-DECIMAL.
                35 PVER-VERSION        PIC S9(004)       BINARY.
                35 PMBE-RFOLGE-NR      PIC S9(004)       BINARY.
                35 PMBE-PGFAELLIG-VON  PIC  X(010).
                35 PMBE-PGFAELLIG-BIS  PIC  X(010).
                35 PMBE-PG-ABR-DAT     PIC  X(010).
                35 PMBE-PG-EINL-DAT    PIC  X(010).
                35 PMBE-GSMT-PROV-DAT  PIC  X(010).
                35 PMBE-KD-BETR        PIC S9(009)       BINARY.
                35 KNR-ZAHLWEISE       PIC S9(004)       BINARY.
                35 PMBE-VSTAT          PIC  X(004).
                35 PMBE-VSTAT-UNR      PIC S9(004)       BINARY.
                35 PMBE-PG-PM-DATEN.
                   40 PMBE-PG-PM-DATEN-LEN
                                       PIC S9(004)       BINARY.
                   40 PMBE-PG-PM-DATEN-TEXT
                                       PIC  X(300).
                35 KNR-MANDANT         PIC S9(004)       BINARY.
080413          35 PMBE-UNR            PIC S9(004)       BINARY.
080413          35 PMBE-SCHICHT-NR     PIC S9(004)       BINARY.
080413          35 PMBE-INKST-DAT      PIC  X(010).
080413          35 KNR-INK-ART         PIC S9(004)       BINARY.
080413          35 PMBE-SAM-NR         PIC S9(009)       BINARY.
080413          35 KNR-RAB-ST          PIC S9(004)       BINARY.
080413          35 PMBE-VVG-KZ         PIC  X(001).
080413          35 KNR-DEPOT-KZ        PIC S9(004)       BINARY.
080413          35 PMBE-PROV-SCHL      PIC  X(005).
080413          35 PMBE-BEW-DAT        PIC  X(010).
080413          35 PMBE-POL-DAT        PIC  X(010).
080413          35 PMBE-BESVV          PIC S9(001)      PACKED-DECIMAL.
080413          35 PMBE-PROV-BEGINN    PIC  X(010).
080413          35 PMBE-PROV-ABLAUF    PIC  X(010).
080413          35 PMBE-KZ-EIGENV      PIC  X(001).
080413          35 PMBE-RATENBEITRAG   PIC S9(009)V9(02) PACKED-DECIMAL.
210217          35 PMBE-PG-RECH-NR     PIC  X(030).
310315          35 PMBE-HIST-UNR       PIC S9(004)       BINARY.
021015          35 PMBE-VP-NR          PIC S9(004)       BINARY.
240418          35 PMBE-GEL-KENNUNG    PIC  X(006).
181119          35 PMBE-BEZUGSJAHR     PIC S9(004)       BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


