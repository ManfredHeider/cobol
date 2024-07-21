      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1968.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1968.CPv  $
      *
      *   Rev 2.2   Jun 12 2013 15:00:34   t13ex27
      *FKI01394: Überarbeitung Ticketfehler
      *
      *   Rev 2.1   27 Mar 2007 10:28:32   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Oct 17 2006 12:45:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06!2.1 ! NEU                       ! IFR      *
      * FKI01394!07.05.13!2.2 ! Überarbeitung Fehlerbe-   !          *
      *         !        !    ! handlung                  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1968-SATZ.
           05  DRKI1968-OTTICKETID            PIC 9(018).
070513     05  DRKI1968-OTTICKETTYP-KTY       PIC 9(004).
           05  DRKI1968-OTTICKETTYP-KNR       PIC 9(004).
           05  DRKI1968-OTAENDGRUND-KTY       PIC 9(004).
           05  DRKI1968-OTAENDGRUND-KNR       PIC 9(004).
           05  DRKI1968-OTERSTELLT            PIC X(026).
           05  DRKI1968-OTERSTELLTVON         PIC X(030).
           05  DRKI1968-OTOKIKUNDENID         PIC 9(018).
           05  DRKI1968-OTHOSTKUNDENID        PIC X(026).
           05  DRKI1968-OTOKIVERTRAGID        PIC 9(018).
           05  DRKI1968-OTHOSTVERTRAGID       PIC X(026).
           05  DRKI1968-OTFEHLER-KTY          PIC 9(004).
           05  DRKI1968-OTFEHLER-KNR          PIC 9(004).
070513*    05  DRKI1968-OTPARAMETER           PIC X(1000).
070513     05  DRKI1968-FEHLER                PIC X(100).
           05  DRKI1968-OTSWVERSION           PIC X(100).
           05  DRKI1968-OTAUSLESEJOBID        PIC 9(018).
      ****************************************************************
