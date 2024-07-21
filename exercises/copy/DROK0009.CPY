      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0009.CPv  $
      *
      *   Rev 2.1   19 Jan 2007 09:49:48   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Mar 17 2006 16:45:58   t13ex27
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
       01 PI-DROK0009.
           05  PI-DROK0009-PROGRAMM            PIC  X(008).
           05  PI-DROK0009-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0009-DATEN.
             10 PI-DROK0009-OTWTICKETID        PIC S9(18)V COMP-3.
      ***
       01 PO-DROK0009.
          05 PO-DROK0009-RC                    PIC  X(003).
             88 PO-DROK0009-RC-OK                        VALUE 'OK '.
             88 PO-DROK0009-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0009-RC-EOF                       VALUE 'EOF'.
          05 PO-DROK0009-DATEN.
           10 PO-DROK0009-OTWSCHLUESSEL        PIC X(30).
           10 PO-DROK0009-OTWWERTALT.
              49 PO-DROK0009-OTWWERTALT-LEN    PIC S9(4) COMP.
              49 PO-DROK0009-OTWWERTALT-TEXT   PIC X(256).
           10 PO-DROK0009-OTWWERTNEU.
              49 PO-DROK0009-OTWWERTNEU-LEN    PIC S9(4) COMP.
              49 PO-DROK0009-OTWWERTNEU-TEXT   PIC X(256).
      *
      ******** Ende Copy-Member **************************************
      *
