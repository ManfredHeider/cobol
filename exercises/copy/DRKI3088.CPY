      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3088.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3088.CPv  $
      *
      *   Rev 2.1   Nov 30 2016 13:58:10   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle
      *
      *   Rev 2.0   Aug 26 2016 08:28:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3088  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!02.05.08!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI3088.
          05 PI-DRKI3088-PROGRAMM               PIC X(008).
          05 PI-DRKI3088-UFKT                   PIC 9(002).

       01 PO-DRKI3088.
          05 PO-DRKI3088-DATEN.
            10 PO-DRKI3088-VBSMITARBEITERNR       PIC S9(9) COMP.
            10 PO-DRKI3088-VBSPERSONENID          PIC  X(0013).
            10 PO-DRKI3088-VBSAUSTRITTSDATUM      PIC  X(0010).

          05 PO-DRKI3088-RC                       PIC  X(003).
             88 PO-DRKI3088-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3088-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI3088-RC-LEER                      VALUE 'LER'.

