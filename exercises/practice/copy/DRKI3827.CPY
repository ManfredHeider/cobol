      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3827.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3827.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:33:06   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:04:44 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3827                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3827                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.06.2020 / 15:04:44 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3827
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3827.
           05  PI-DRKI3827-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3827-AKOANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3827-AKOPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3827-AKOTARIFTEIL-ID   PIC  X(0010).
             10 PI-DRKI3827-AKOVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3827-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3827-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3827-AKOGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOSTICHTAG   PIC  X(010).
             10 PI-DRKI3827-AKOSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOTARIFNAME   PIC  X(0040).
             10 PI-DRKI3827-AKOVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3827-AKOZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3827-NI1.
           05 PI-DRKI3827-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3827-NI7.
           05 PI-DRKI3827-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3827.
           05  PO-DRKI3827-RC                     PIC  X(003).
               88 PO-DRKI3827-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3827-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

