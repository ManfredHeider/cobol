      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2205.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI2205.CPv  $
      *
      *   Rev 2.10   Apr 21 2015 14:02:20   t13ex27
      *AKI01352: VB-Nummer Prüfung 'neu' gestalten
      *
      *   Rev 2.9   Sep 03 2012 10:20:58   t13ex14
      *AKI01155: VerwFond berein. 1: Unterprogramme
      *
      *   Rev 2.8   Aug 15 2012 13:37:42   t13ex14
      *AKI01155: VerwFond berein. 1: Unterprogramme
      *
      *   Rev 2.7   12 Dec 2007 13:47:22   t13ex39
      *AKL00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA
      *
      *   Rev 2.6   03 Dec 2007 11:29:16   t13ex39
      *AKL00664: U P K I 0 2 0 9
      *
      *   Rev 2.0   02 Oct 2007 14:02:18   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2205                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FüR DAS UPRO UPKI0209                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00644!01.10.07! 1.0! NEU                       ! HORNUNG  *
260612* AKI01155!26.06.12!    ! neues Feld PI-DRKI2205-GES! WMI      *
      *         !        !    ! und VERARBKZ              !          *
      * AKI01352!03.03.15!2.10! Neugestaltung VB-Pr}fung  ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2205
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRKI2205.
           05  PI-DRKI2205-DATEN.
             10 PI-DRKI2205-VB-NUMMER          PIC 9(07).
260612*      10 PI-DRKI2205-VERT-ID            PIC X(13).
260612       10 PI-DRKI2205-RUF-PGM            PIC X(08).
260612       10 PI-DRKI2205-LFDNR              PIC S9(04) COMP.
260612       10 PI-DRKI2205-GES                PIC 9(02).
             10 PI-DRKI2205-VNR                PIC 9(15).
             10 PI-DRKI2205-TAGESDATUM         PIC x(010).
 
      *
       01  PO-DRKI2205.
           05  PO-DRKI2205-RC                 PIC  X(003).
               88 PO-DRKI2205-RC-OK                        VALUE 'OK '.
      * kein VN in VERTAGROLLEBEZ
               88 PO-DRKI2205-RC-VB-NOT-FOUND              VALUE 'VBN'.
 
           05  PO-DRKI2205-GRUND              PIC X(01).
      * durch UPKI0209 ermittelt
               88 PO-DRKI2205-NOT-NUMERIC        VALUE 'U'.
               88 PO-DRKI2205-GLEICH-ZEROE       VALUE 'Z'.
               88 PO-DRKI2205-FALSCHER-MANDANT   VALUE 'N'.
               88 PO-DRKI2205-VB-NICHT-VORHANDEN VALUE 'V'.
 
      * durch ZWVB9992 ermittelt
               88 PO-DRKI2205-VB-NR-FALSCH       VALUE 'F'.
               88 PO-DRKI2205-VB-AUSGESCHIEDEN   VALUE 'A'.
               88 PO-DRKI2205-VB-IST-VM          VALUE 'M'.
               88 PO-DRKI2205-VBSYSTEM-NICHT-AKT VALUE 'S'.
               88 PO-DRKI2205-VB-OK              VALUE 'J'.
 
           05  PO-DRKI2205-VB-NR              PIC S9(09) COMP.
 
      * nur bei DEUBA verwendet
           05  PO-DRKI2205-VRB-VB-NR          PIC 9(07) .
      *
      ******* ENDE COPY-MEMBER **************************************

