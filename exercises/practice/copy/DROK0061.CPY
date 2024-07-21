      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0061.CPv  $
      *
      *   Rev 2.1   19 Jan 2007 09:50:04   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Dec 08 2006 10:57:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0061                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTOK0008                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.12.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00557!08.12.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DROK0061 - SELECT ALL AUF TABELLE OKITICKET       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DROK0061.
           05  PI-DROK0061-DATEN.
               10  PI-DROK0061-OTTICKETID        PIC  9(018).
      *
       01  PO-DROK0061.
           05  PO-DROK0061-RC                    PIC  X(003).
               88  PO-DROK0061-RC-OK                  VALUE 'OK '.
               88  PO-DROK0061-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DROK0061-NI-OKITICKET.
               10 PO-DROK0061-NI-OTTICKETID      PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTTICKETTYP-KTY PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTTICKETTYP-KNR PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTAENDGRUND-KTY PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTAENDGRUND-KNR PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTERSTELLT      PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTERSTELLTVON   PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTOKIKUNDENID   PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTHOSTKUNDENID  PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTOKIVERTRAGID  PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTHOSTVERTRAGID PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTSWVERSION     PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTSTATUS-KTY    PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTSTATUS-KNR    PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTAUSLESEJOBID  PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTFEHLER-KTY    PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTFEHLER-KNR    PIC S9(004) COMP.
               10 PO-DROK0061-NI-OTPARAMETER     PIC S9(004) COMP.
           05  PO-DROK0061-DCL-OKITICKET.
               10 PO-DROK0061-OTTICKETID         PIC S9(018)V COMP-3.
               10 PO-DROK0061-OTTICKETTYP-KTY    PIC S9(004) COMP.
               10 PO-DROK0061-OTTICKETTYP-KNR    PIC S9(004) COMP.
               10 PO-DROK0061-OTAENDGRUND-KTY    PIC S9(004) COMP.
               10 PO-DROK0061-OTAENDGRUND-KNR    PIC S9(004) COMP.
               10 PO-DROK0061-OTERSTELLT         PIC  X(026).
               10 PO-DROK0061-OTERSTELLTVON      PIC  X(030).
               10 PO-DROK0061-OTOKIKUNDENID      PIC S9(018)V COMP-3.
               10 PO-DROK0061-OTHOSTKUNDENID     PIC  X(013).
               10 PO-DROK0061-OTOKIVERTRAGID     PIC S9(018)V COMP-3.
               10 PO-DROK0061-OTHOSTVERTRAGID    PIC  X(013).
               10 PO-DROK0061-OTSWVERSION.
                  15 PO-DROK0061-OTSWVERSION-LEN    PIC S9(004) COMP.
                  15 PO-DROK0061-OTSWVERSION-TEXT   PIC  X(100).
               10 PO-DROK0061-OTSTATUS-KTY       PIC S9(004) COMP.
               10 PO-DROK0061-OTSTATUS-KNR       PIC S9(004) COMP.
               10 PO-DROK0061-OTAUSLESEJOBID     PIC S9(018)V COMP-3.
               10 PO-DROK0061-OTFEHLER-KTY       PIC S9(004) COMP.
               10 PO-DROK0061-OTFEHLER-KNR       PIC S9(004) COMP.
               10 PO-DROK0061-OTPARAMETER.
                  15 PO-DROK0061-OTPARAMETER-LEN    PIC S9(004) COMP.
                  15 PO-DROK0061-OTPARAMETER-TEXT   PIC  X(1000).
      ****
      **** ENDE COPY DROK0061 ****************************************
      ****
