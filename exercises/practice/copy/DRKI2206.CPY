      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2206.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI2206.CPv  $
      *
      *   Rev 2.10   Sep 03 2012 10:21:06   t13ex14
      *AKI01155: VerwFond berein. 1: Unterprogramme
      *
      *   Rev 2.9   Aug 15 2012 13:37:50   t13ex14
      *AKI01155: VerwFond berein. 1: Unterprogramme
      *
      *   Rev 2.8   25 Jan 2008 10:07:16   t13ex39
      *AKL00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA 
      *
      *   Rev 2.7   12 Dec 2007 13:47:16   t13ex39
      *AKL00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA 
      *
      *   Rev 2.6   03 Dec 2007 11:29:20   t13ex39
      *AKL00664: U P K I 0 2 0 9   
      *
      *   Rev 2.5   17 Oct 2007 09:15:50   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.4   04 Oct 2007 13:35:58   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.3   04 Oct 2007 13:33:08   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.2   04 Oct 2007 13:27:48   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.0   02 Oct 2007 14:04:40   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2206                                     *
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
      * AKI00877!25.01.05! 1.0! NEU                       ! HORNUNG  *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKL00877!25.01.08! 1.1! überflüssigen Source entf.! HORNUNG  *
260612* AKI01155!26.06.12!    ! neues Feld PI-DRKI2205-GES! WMI      *
      *         !        !    ! und VERARBKZ              !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2206
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRKI2206.
           05  PI-DRKI2206-DATEN.
             10 PI-DRKI2206-VB-NUMMER          PIC 9(07).
260612*      10 PI-DRKI2206-VERT-ID            PIC X(13).
260612       10 PI-DRKI2206-RUF-PGM            PIC X(08).
260612       10 PI-DRKI2206-LFDNR              PIC S9(04) COMP.
260612       10 PI-DRKI2206-GES                PIC 9(02).
260612       10 PI-DRKI2206-STATUS             PIC X(01).
             10 PI-DRKI2206-VNR                PIC 9(15).

      *
       01  PO-DRKI2206.
           05  PO-DRKI2206-RC                 PIC  X(003).
               88 PO-DRKI2206-RC-OK                        VALUE 'OK '.
      * KEIN VN IN VERTAGROLLEBEZ
               88 PO-DRKI2206-RC-VB-NOT-FOUND             VALUE 'VBN'.

           05  PO-DRKI2206-VRB-VB-NR          PIC 9(07) .

           05  PO-DRKI2206-GRUND              PIC X(01).
               88 PO-DRKI2206-VB-NR-FALSCH       VALUE 'F'.
               88 PO-DRKI2206-VB-AUSGESCHIEDEN   VALUE 'A'.
               88 PO-DRKI2206-VB-NICHT-VORHANDEN VALUE 'V'.
               88 PO-DRKI2206-VB-IST-VM          VALUE 'M'.
               88 PO-DRKI2206-FALSCHER-MANDANT   VALUE 'N'.
               88 PO-DRKI2206-NOT-NUMERIC        VALUE 'U'.
               88 PO-DRKI2206-GLEICH-ZEROE       VALUE 'Z'.
               88 PO-DRKI2206-VBSYSTEM-NICHT-AKT VALUE 'S'.

           05  PO-DRKI2206-VB-NR              PIC S9(09) COMP.
      *

