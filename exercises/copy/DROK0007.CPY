      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0007.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:54:28   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Mar 17 2006 16:39:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0007.
           05  PI-DROK0007-PROGRAMM                PIC  X(008).
           05  PI-DROK0007-UFKT                    PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT

           05 PI-DROK0007-DATEN.
              10 PI-DROK0007-OTSTATUS-KTY          PIC S9(4) COMP.
              10 PI-DROK0007-OTSTATUS-KNR          PIC S9(4) COMP.
              10 PI-DROK0007-OTTICKETTYP-KTY       PIC S9(4) COMP.
      ***
       01 PO-DROK0007.
          05 PO-DROK0007-RC                        PIC  X(003).
             88 PO-DROK0007-RC-OK                        VALUE 'OK '.
             88 PO-DROK0007-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0007-RC-EOF                       VALUE 'EOF'.

          05 PO-DROK0007-DATEN.
              10 PO-DROK0007-OTTICKETID            PIC S9(18)V COMP-3.
              10 PO-DROK0007-OTTICKETTYP-KNR       PIC S9(4) COMP.
              10 PO-DROK0007-OTAENDGRUND-KTY       PIC S9(4) COMP.
              10 PO-DROK0007-OTAENDGRUND-KNR       PIC S9(4) COMP.
              10 PO-DROK0007-OTERSTELLT            PIC X(26).
              10 PO-DROK0007-OTERSTELLTVON         PIC X(30).
              10 PO-DROK0007-OTOKIKUNDENID         PIC S9(18)V COMP-3.
              10 PO-DROK0007-OTHOSTKUNDENID        PIC X(13).
              10 PO-DROK0007-OTOKIVERTRAGID        PIC S9(18)V COMP-3.
              10 PO-DROK0007-OTHOSTVERTRAGID       PIC X(13).
              10 PO-DROK0007-OTSWVERSION.
                 49 PO-DROK0007-OTSWVERSION-LEN    PIC S9(4) COMP.
                 49 PO-DROK0007-OTSWVERSION-TEXT   PIC X(100).
              10 PO-DROK0007-OTAUSLESEJOBID        PIC S9(18)V COMP-3.
              10 PO-DROK0007-OTFEHLER-KTY          PIC S9(4) COMP.
              10 PO-DROK0007-OTFEHLER-KNR          PIC S9(4) COMP.
              10 PO-DROK0007-OTPARAMETER.
                 49 PO-DROK0007-OTPARAMETER-LEN    PIC S9(4) COMP.
                 49 PO-DROK0007-OTPARAMETER-TEXT   PIC X(1000).
      *
      ******** Ende Copy-Member **************************************
