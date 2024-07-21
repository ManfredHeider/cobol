      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2993.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2993.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:12:12   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:10   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:19:45 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2993                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2993                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         31.07.2015 / 15:51:06 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2993
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2993.
           05  PI-DRKI2993-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2993-DKZVERTRAGSID   PIC  X(0013).
END   *
 
 
           05  PI-DRKI2993-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2993-DKZAPANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2993-DKZAPEMAIL.
                49 PI-DRKI2993-DKZAPEMAIL-LEN PIC S9(4) COMP.
                49 PI-DRKI2993-DKZAPEMAIL-TEXT PIC X(128).
             10 PI-DRKI2993-DKZAPFUNKTION.
                49 PI-DRKI2993-DKZAPFUNKTION-LEN PIC S9(4) COMP.
                49 PI-DRKI2993-DKZAPFUNKTION-TEXT PIC X(50).
             10 PI-DRKI2993-DKZAPGEBDAT   PIC  X(010).
             10 PI-DRKI2993-DKZAPNACHNAME   PIC  X(0030).
             10 PI-DRKI2993-DKZAPTITEL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2993-DKZAPVORNAME   PIC  X(0030).
             10 PI-DRKI2993-DKZFAXNR   PIC  X(0030).
             10 PI-DRKI2993-DKZINHABER.
                49 PI-DRKI2993-DKZINHABER-LEN PIC S9(4) COMP.
                49 PI-DRKI2993-DKZINHABER-TEXT PIC X(100).
             10 PI-DRKI2993-DKZMOBILNR   PIC  X(0030).
             10 PI-DRKI2993-DKZPOSTFACH   PIC  X(0010).
             10 PI-DRKI2993-DKZPOSTFACHORT   PIC  X(0040).
             10 PI-DRKI2993-DKZPOSTFACHPLZ   PIC S9(9) COMP.
             10 PI-DRKI2993-DKZRECHTSFORM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2993-DKZTELNR   PIC  X(0030).
END   *
 
      *
       01  PO-DRKI2993.
           05  PO-DRKI2993-RC                     PIC  X(003).
               88 PO-DRKI2993-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2993-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

