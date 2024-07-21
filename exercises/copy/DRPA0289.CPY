      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0289.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0289.CPv  $
      *
      *   Rev 2.4   May 14 2018 11:11:46   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.3   Mar 27 2017 16:46:48   t13ae46
      *APA02353: Neuer Produktpartner TeleSon - PEG
      *
      *   Rev 2.2   Jul 16 2015 14:59:56   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.1   Dec 17 2013 11:15:56   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG
      *
      *   Rev 2.0   Feb 27 1999 14:28:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:46:16   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:31:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0289                                      *
      *    --------------------------------------------------------   *
      *    PMPE-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.05.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!29.07.97!    ! EINBAU GESAMTPROVISIONS-  ! WOLFF    *
      *         !        !    ! DATUM                     !          *
      * AFPA0012!29.08.97!    ! ÄNDERUNG VON FELDNAMEN    ! WOLFF    *
      * AFPA0012!13.10.97!    ! PG-DATEN UMBENANNT, KTY   ! WOLFF    *
      *         !        !    ! ENTFERNT                  !          *
      * APA01526!19.03.13!2.1 ! NEUE ATTRIBUTE HINZU      ! STORCH   *
      * APA01932!27.03.15!2.2 ! NEUE ATTRIBUTE HINZU      ! STORCH   *
      * APA02353!20.02.17!2.3 ! NEUE ATTRIBUTE HINZU      ! WANG     *
      * APA02479!24.04.18!2.4 ! GENERALI LEBEN            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PMPE.                   *
      *                                                              *
      ****************************************************************
      *
             30 PMPE-DATEN.
                35 PMPG-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PMPG-VERSION        PIC S9(004)       BINARY.
                35 PMPG-LFD-NR-VNR     PIC S9(009)       BINARY.
                35 PDTA-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PGEB-GES            PIC S9(004)       BINARY.
                35 PMPE-VNR            PIC  X(015).
                35 PMPE-VERTR-BEGINN   PIC  X(010).
                35 PMPE-VERTR-ABLAUF   PIC  X(010).
                35 PMPE-FAELLIG-VON    PIC  X(010).
                35 PMPE-FAELLIG-BIS    PIC  X(010).
                35 PMPE-PG-VBNR        PIC S9(009)       BINARY.
                35 PMPE-KUNDE-NAME     PIC  X(025).
                35 PMPE-KUNDE-VNAME    PIC  X(020).
                35 PMPE-GSMT-PROV-DAT  PIC  X(010).
                35 PMPE-PG-ABR-DAT     PIC  X(010).
                35 PMPE-PG-EINL-DAT    PIC  X(010).
                35 PMPE-PIN            PIC S9(009)       BINARY.
                35 KNR-ZAHLWEISE       PIC S9(004)       BINARY.
                35 KNR-WAEHRUNG        PIC S9(004)       BINARY.
                35 PMPE-VSTAT          PIC  X(004).
                35 PMPE-PG-PM-DATEN.
                   40 PMPE-PG-PM-DATEN-LEN
                                       PIC S9(004)       BINARY.
                   40 PMPE-PG-PM-DATEN-TEXT
                                       PIC  X(300).
190213          35 PMPE-BZ-NAME        PIC  X(025).
190213          35 PMPE-BZ-VNAME       PIC  X(020).
190213          35 PMPE-VP-NAME        PIC  X(025).
190213          35 PMPE-VP-VNAME       PIC  X(020).
190213          35 PMPE-KZ-EIGENV      PIC  X(001).
190213          35 KNR-FEIV-KZ         PIC S9(004)       BINARY.
190213          35 KNR-VERM-GES        PIC S9(004)       BINARY.
190213          35 PMPE-UMW-NR         PIC S9(015)       PACKED-DECIMAL.
190213          35 PMPE-UNR            PIC S9(004)       BINARY.
190213          35 PMPE-SCHICHT-NR     PIC S9(004)       BINARY.
190213          35 PMPE-INKST-DAT      PIC  X(010).
190213          35 KNR-INK-ART         PIC S9(004)       BINARY.
190213          35 PMPE-SAM-NR         PIC S9(009)       BINARY.
190213          35 KNR-RAB-ST          PIC S9(004)       BINARY.
190213          35 PMPE-VVG-KZ         PIC  X(001).
190213          35 KNR-DEPOT-KZ        PIC S9(004)       BINARY.
190213          35 PMPE-PROV-SCHL      PIC  X(005).
190213          35 PMPE-BEW-DAT        PIC  X(010).
190213          35 PMPE-URSPR-POL-DAT  PIC  X(010).
190213          35 PMPE-POL-DAT        PIC  X(010).
190213          35 PMPE-BESVV          PIC S9(001)       PACKED-DECIMAL.
190213          35 PMPE-VN-STRASSE     PIC  X(035).
190213          35 PMPE-VN-PLZ         PIC  X(005).
190213          35 PMPE-VN-ORT         PIC  X(035).
190213          35 PMPE-RATENBEITRAG   PIC S9(009)V9(02) PACKED-DECIMAL.
200217          35 PMPE-PG-RECH-NR     PIC  X(030).
270315          35 PMPE-HIST-UNR       PIC S9(004)       BINARY.
200217          35 KNR-TLSN-PARTNER    PIC S9(004)       BINARY.
240418          35 PMPE-GEL-KENNUNG    PIC  X(006).
      *
      ******* ENDE COPY-MEMBER **************************************

