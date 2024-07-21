      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3681.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3681.CPv  $
      *
      *   Rev 2.1   Sep 02 2019 13:16:16   u130176
      *AKI01689: DSGVO:Löschverfahren für Tabelle VBSTAMMDATEN
      *
      *   Rev 2.0   Jun 17 2019 10:07:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3681                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJKI0040                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRENZEL                                        *
      *    DATUM:    24.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI01689!24.06.19!2.1 ! NEU                       ! T13EX27   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3681.
           05  PI-DRKI3681-PROGRAMM              PIC  X(008).
           05  PI-DRKI3681-UFKT                  PIC  9(002).
      *
       01  PO-DRKI3681.
           05  PO-DRKI3681-RC                    PIC  X(003).
               88  PO-DRKI3681-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3681-RC-LEER                VALUE 'LER'.
               88  PO-DRKI3681-RC-EOF                 VALUE 'EOF'.
           05  PO-DRKI3681-DATEN.
             10 PO-DRKI03681-VBSPERSONENID       PIC  X(013).
             10 PO-DRKI3681-VBSMITARBEITERNR     PIC S9(9) COMP.
             10 PO-DRKI3681-VBSMANDANTNR         PIC S9(9) COMP.
             10 PO-DRKI3681-VBSANREDE            PIC S9(4) COMP.
             10 PO-DRKI3681-VBSNAME              PIC  X(0040).
             10 PO-DRKI3681-VBSVORNAME           PIC  X(0020).
             10 PO-DRKI3681-VBSGEBGRUENDDAT      PIC  X(010).
             10 PO-DRKI3681-VBSSTRUKTURSTUFE     PIC S9(9) COMP.
             10 PO-DRKI3681-VBSDIREKTION         PIC S9(4) COMP.
             10 PO-DRKI3681-VBSAUSTRITTSDATUM    PIC  X(010).

