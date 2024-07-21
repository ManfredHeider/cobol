      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2977.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2977.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:11:42   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:44   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 12:37:45 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2977                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2977                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.01.2015 / 12:37:45 / T13EX73
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2977
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2977.
           05 PI-DRKI2977-PROGRAMM              PIC  X(008).
           05 PI-DRKI2977-UFKT-CODE             PIC  9(002).
                88 PO-DRKI2977-UFKT-FIRST               VALUE 1.
                88 PO-DRKI2977-UFKT-NEXT                VALUE 2.
                88 PO-DRKI2977-UFKT-QUIT                VALUE 3.
                88 PO-DRKI2977-UFKT-UPDATE              VALUE 4.
      *
           05  PI-DRKI2977-KEY.
                10 PI-DRKI2977-G90VMANDANT      PIC S9(9) COMP.
      *
       01  PO-DRKI2977.
           05  PO-DRKI2977-RC                   PIC  X(003).
                88 PO-DRKI2977-RC-OK                    VALUE 'OK '.
                88 PO-DRKI2977-RC-LEER                  VALUE 'LER'.
                88 PO-DRKI2977-RC-EOF                   VALUE 'EOF'.
      *
      *
           05  'XX'-DATEN.
                10 'XX'-G90VABRGEBSATZ          PIC S9(1)V9(2) COMP-3.
                10 'XX'-G90VABSCHLVBNR          PIC S9(9) COMP.
                10 'XX'-G90VABWBZKZ             PIC  X(0001).
                10 'XX'-G90VABWBZNNAME          PIC  X(0030).
                10 'XX'-G90VABWBZORT            PIC  X(0030).
                10 'XX'-G90VABWBZPLZ            PIC S9(9) COMP.
                10 'XX'-G90VABWBZSTRASSE        PIC  X(0040).
                10 'XX'-G90VABWBZVNAME          PIC  X(0030).
                10 'XX'-G90VAENDKZ              PIC  X(0002).
                10 'XX'-G90VBEGINN              PIC  X(010).
                10 'XX'-G90VEINRGEBBETR         PIC S9(8)V9(2) COMP-3.
                10 'XX'-G90VENDE                PIC  X(010).
                10 'XX'-G90VFACTGEBSATZ         PIC S9(1)V9(2) COMP-3.
                10 'XX'-G90VFACTANKBETR         PIC S9(10)V9(2) COMP-3.
                10 'XX'-G90VGESID               PIC S9(4) COMP.
                10 'XX'-G90VIBAN                PIC  X(0034).
                10 'XX'-G90VINFOSPERRE          PIC  X(0001).
                10 'XX'-G90VMANDANT             PIC S9(9) COMP.
                10 'XX'-G90VMANDATREFNR         PIC  X(0035).
                10 'XX'-G90VPRODUKTART          PIC S9(4) COMP.
                10 'XX'-G90VPROGANZRECHN        PIC S9(9) COMP.
                10 'XX'-G90VPROGBISDAT          PIC  X(010).
                10 'XX'-G90VPROGGESUMS          PIC S9(8)V9(2) COMP-3.
                10 'XX'-G90VPROGVONDAT          PIC  X(010).
                10 'XX'-G90VVERTRAGSNR          PIC  X(0015).
      *
      ******* ENDE COPY-MEMBER ***************************************

