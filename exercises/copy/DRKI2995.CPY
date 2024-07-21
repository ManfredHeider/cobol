      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2995.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2995.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:12:26   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:20   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:19:49 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2995                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2995                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         31.07.2015 / 15:51:11 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2995
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2995.
           05  PI-DRKI2995-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2995-DKZVERTRAGSID   PIC  X(0013).
END   *
 
 
           05  PI-DRKI2995-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2995-DKZAPANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2995-DKZAPEMAIL.
                49 PI-DRKI2995-DKZAPEMAIL-LEN PIC S9(4) COMP.
                49 PI-DRKI2995-DKZAPEMAIL-TEXT PIC X(128).
             10 PI-DRKI2995-DKZAPFUNKTION.
                49 PI-DRKI2995-DKZAPFUNKTION-LEN PIC S9(4) COMP.
                49 PI-DRKI2995-DKZAPFUNKTION-TEXT PIC X(50).
             10 PI-DRKI2995-DKZAPGEBDAT   PIC  X(010).
             10 PI-DRKI2995-DKZAPNACHNAME   PIC  X(0030).
             10 PI-DRKI2995-DKZAPTITEL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2995-DKZAPVORNAME   PIC  X(0030).
             10 PI-DRKI2995-DKZFAXNR   PIC  X(0030).
             10 PI-DRKI2995-DKZINHABER.
                49 PI-DRKI2995-DKZINHABER-LEN PIC S9(4) COMP.
                49 PI-DRKI2995-DKZINHABER-TEXT PIC X(100).
             10 PI-DRKI2995-DKZMOBILNR   PIC  X(0030).
             10 PI-DRKI2995-DKZPOSTFACH   PIC  X(0010).
             10 PI-DRKI2995-DKZPOSTFACHORT   PIC  X(0040).
             10 PI-DRKI2995-DKZPOSTFACHPLZ   PIC S9(9) COMP.
             10 PI-DRKI2995-DKZRECHTSFORM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2995-DKZTELNR   PIC  X(0030).
END   *
 
 
      *
       01  PO-DRKI2995.
           05  PO-DRKI2995-RC                     PIC  X(003).
               88 PO-DRKI2995-RC-OK               VALUE 'OK '.
               88 PO-DRKI2995-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

