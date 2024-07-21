      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2252.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2252.CPv  $
      *
      *   Rev 2.3   Mar 02 2015 16:23:24   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.2   Sep 01 2014 10:35:06   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   09 Jun 2008 15:58:58   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben OZPs
      *
      *   Rev 2.0   07 May 2008 10:35:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2252                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2252                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.02.2015 / 16:11:15 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2252
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2252.
           05  PI-DRKI2252-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2252-LFBFONDSKUERZEL   PIC  X(0002).
END   *
 
 
           05  PI-DRKI2252-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2252-LFBFONDSBEZEICH   PIC  X(0050).
             10 PI-DRKI2252-LFBGUELTIGAB   PIC  X(010).
             10 PI-DRKI2252-LFBGUELTIGBIS   PIC  X(010).
             10 PI-DRKI2252-LFBISIN   PIC  X(0012).
             10 PI-DRKI2252-LFBWKN   PIC  X(0006).
END   *
 
 
      *
       01  PO-DRKI2252.
           05  PO-DRKI2252-RC                     PIC  X(003).
               88 PO-DRKI2252-RC-OK               VALUE 'OK '.
               88 PO-DRKI2252-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

