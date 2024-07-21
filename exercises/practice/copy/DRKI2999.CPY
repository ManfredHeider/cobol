      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2999.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2999.CPv  $
      *
      *   Rev 2.3   Jun 30 2020 09:53:00   t13ex78
      *AKI01780: Standard-OZP ZTKI0503
      *
      *   Rev 2.2   Sep 02 2015 15:12:48   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:40   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:20:47 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2999                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2999                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.05.2020 / 07:36:54 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2999
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2999.
           05  PI-DRKI2999-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2999-DVZVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2999.
           05  PO-DRKI2999-RC                     PIC  X(003).
               88 PO-DRKI2999-RC-OK               VALUE 'OK '.
               88 PO-DRKI2999-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI2999-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2999-DVZABRGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PO-DRKI2999-DVZEINRGEBBETR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2999-DVZFACTANKBETR   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI2999-DVZFACTGEBSATZ   PIC S9(1)V9(2) COMP-3.
             10 PO-DRKI2999-DVZPRODUKTART   PIC S9(4) COMP.
             10 PO-DRKI2999-DVZPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2999-DVZPROGANZRECHN   PIC S9(9) COMP.
             10 PO-DRKI2999-DVZPROGANZRECHNMM   PIC S9(9) COMP.
             10 PO-DRKI2999-DVZPROGBISDAT   PIC  X(010).
             10 PO-DRKI2999-DVZPROGBISDATMM   PIC  X(010).
             10 PO-DRKI2999-DVZPROGGESUMS   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2999-DVZPROGGESUMSMM   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2999-DVZPROGVONDAT   PIC  X(010).
             10 PO-DRKI2999-DVZPROGVONDATMM   PIC  X(010).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

