      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1658.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1658.CPv  $
      *
      *   Rev 2.2   Jun 27 2006 12:00:18   t13ex27
      *AKI00579: Bestellung VB-Magazin bei zukünftigem Austrittsdatum
      *
      *   Rev 2.1   Oct 25 2004 12:06:10   t13ex27
      *AKI00399: Abo-Funktion (Einverständniserklärung) im Bestellv
      *organg
      *
      *   Rev 2.0   Mar 23 2004 10:57:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00399!06.11.04!2.1 ! NEUERSTELLUNG             ! IFR      *
      * AKI00579!05.05.06!2.2 ! Neubehandlung des         !I.Frenzel *
      *         !        !    ! Austrittsdatums           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI1658.
          05 PI-DRKI1658-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1658-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1658-DATEN.
            07 PI-DRKI1658-ADMABESTELLAGENT       PIC  X(001).
050506      07 PI-DRKI1658-ADMAAUSTRITTSDATUM     PIC  X(010).
      *
       01 PO-DRKI1658.

          05 PO-DRKI1658-RC                       PIC  X(003).
             88 PO-DRKI1658-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1658-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1658-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI1658-DATEN.
            07 PO-DRKI1658-ADMAPERSONENID         PIC X(013).
            07 PO-DRKI1658-ADMAAUSTRITTSDATUM     PIC X(010).

      ***** Ende Copy-Member DRKI1658 *****************************
