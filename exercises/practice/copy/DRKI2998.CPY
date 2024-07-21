      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2998.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2998.CPv  $
      *
      *   Rev 2.3   Jun 30 2020 09:52:40   t13ex78
      *AKI01780: Standard-OZP ZTKI0503
      *
      *   Rev 2.2   Sep 02 2015 15:12:42   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:34   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:20:45 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2998                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2998                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.05.2020 / 07:36:52 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2998
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2998.
           05  PI-DRKI2998-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2998-DVZVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2998-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2998-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2998-DVZABRGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2998-DVZEINRGEBBETR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2998-DVZFACTANKBETR   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2998-DVZFACTGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI2998-DVZPRODUKTART   PIC S9(4) COMP.
             10 PI-DRKI2998-DVZPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2998-DVZPROGANZRECHN   PIC S9(9) COMP.
             10 PI-DRKI2998-DVZPROGANZRECHNMM   PIC S9(9) COMP.
             10 PI-DRKI2998-DVZPROGBISDAT   PIC  X(010).
             10 PI-DRKI2998-DVZPROGBISDATMM   PIC  X(010).
             10 PI-DRKI2998-DVZPROGGESUMS   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2998-DVZPROGGESUMSMM   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2998-DVZPROGVONDAT   PIC  X(010).
             10 PI-DRKI2998-DVZPROGVONDATMM   PIC  X(010).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2998-NI1.
           05 PI-DRKI2998-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2998-NI7.
           05 PI-DRKI2998-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI2998.
           05  PO-DRKI2998-RC                     PIC  X(003).
               88 PO-DRKI2998-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2998-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

