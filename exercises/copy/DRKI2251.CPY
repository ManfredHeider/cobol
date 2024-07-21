      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2251.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2251.CPv  $
      *
      *   Rev 2.3   Mar 02 2015 16:23:20   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.2   Sep 01 2014 10:34:56   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   09 Jun 2008 15:58:52   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben OZPs
      *
      *   Rev 2.0   07 May 2008 10:35:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2251                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2251                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.02.2015 / 16:11:13 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2251
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2251.
           05  PI-DRKI2251-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2251-LFBFONDSKUERZEL   PIC  X(0002).
END   *
 
 
           05  PI-DRKI2251-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2251-LFBFONDSBEZEICH   PIC  X(0050).
             10 PI-DRKI2251-LFBGUELTIGAB   PIC  X(010).
             10 PI-DRKI2251-LFBGUELTIGBIS   PIC  X(010).
             10 PI-DRKI2251-LFBISIN   PIC  X(0012).
             10 PI-DRKI2251-LFBWKN   PIC  X(0006).
END   *
 
      *
       01  PO-DRKI2251.
           05  PO-DRKI2251-RC                     PIC  X(003).
               88 PO-DRKI2251-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2251-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

