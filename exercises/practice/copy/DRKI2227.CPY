      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2227.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2227.CPv  $
      *
      *   Rev 2.1   Jan 11 2008 18:28:12   t13ex51
      *AKL00664: Allfinanz: VBI-Schnittstelle
      *
      *   Rev 2.0   Jan 11 2008 11:17:44   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2227  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                         *
      *    DATUM:    05.10.2007                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!11.01.08! 01 ! NEUERSTELLUNG             ! HRO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2227.
          05 PI-DRKI2227-PROGRAMM               PIC X(008).
          05 PI-DRKI2227-UFKT                   PIC 9(002).
          05 PI-DRKI2227-DATEN.
            10 PI-DRKI2227-VEZVERTRAGSID        PIC X(13).


       01 PO-DRKI2227.
          05 PO-DRKI2227-DATEN.
            10 PO-DRKI2227-VEZVETLFDNR          PIC S9(4) COMP.
            10 PO-DRKI2227-VEZPRODUKTID         PIC S9(4) COMP.
            10 PO-DRKI2227-VEZNJBEITRAG         PIC S9(8)V9(2) COMP-3.

          05 PO-DRKI2227-RC                       PIC  X(003).
             88 PO-DRKI2227-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2227-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2227-RC-LER                       VALUE 'LER'.

