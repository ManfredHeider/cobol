      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0548.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0548.CPv  $
      *
      *   Rev 2.5   Jan 22 2021 13:42:40   u130176
      *AKI01824: STVD0060: Advocard-Jahresnettobeitrag aus VERSSACH
      *TARIF
      *
      *   Rev 2.4   Dec 16 2019 10:50:18   t13ac23
      *AKI01738: Advocard Jahresbeiträge Nettobeitrag ausweisen (VB
      *I, KLB...)
      *
      *   Rev 2.3   Nov 19 2019 10:52:42   t13ac23
      *AKI01738: Advocard Jahresbeiträge Nettobeitrag ausweisen (VB
      *I, KLB...)
      *
      *   Rev 2.2   Oct 17 2012 16:08:52   t13ex27
      *AKI01135: Kundendaten physisch löschen-Nacharbeiten
      *
      *   Rev 2.1   Oct 01 1999 09:04:12   T13AE21
      *AKI00029
      *
      *   Rev 2.0   Feb 27 1999 12:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   07 Aug 1998 14:47:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0548                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    15.09.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00029!15.09.99! 01 ! NEUERSTELLUNG             ! PHILIPP  *
      * AKI01135!20.09.12!2.2 ! Korrektur RC              !I. Frenzel*
      * AKI01738!14.11.19!2.3 ! Erweiterung um            ! Detmar   *
      *         !        !2.4 ! neue Felder               !          *
      * AKI01824!13.01.21!2.5 ! SATRISIKNETGES hinzu      ! Lorson   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0548                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0548.
          05 PI-DRKI0548-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0548-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0548-DATEN.
            10 PI-DRKI0548-SATVERTRAGSID          PIC X(013).
      *
       01 PO-DRKI0548.

          05 PO-DRKI0548-RC                       PIC  X(003).
             88 PO-DRKI0548-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0548-RC-LER                   VALUE 'LER'.
             88 PO-DRKI0548-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0548-DATEN.
            10 PO-DRKI0548-SATJAHRESBEITRAG   PIC S9(09)V9(2) COMP-3.
            10 PO-DRKI0548-SATJAHRBEITRWKZ    PIC X(01).
            10 PO-DRKI0548-SATVERSTARLFDNR    PIC S9(4) COMP.
            10 PO-DRKI0548-SATZWEIGTEXT       PIC X(3).
            10 PO-DRKI0548-SATJHRBTRGNETTO    PIC S9(009)v99 COMP-3.
            10 PO-DRKI0548-SATRISIKNETGES     PIC S9(009)v99 COMP-3.

      *
      ***** Ende Copy-Member DRKI0548 *****************************
      *---------------------------------------------------------------*



