      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2994.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2994.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:12:20   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:14   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:19:47 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2994                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2994                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         31.07.2015 / 15:51:09 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2994
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2994.
           05  PI-DRKI2994-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2994-DKZVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2994.
           05  PO-DRKI2994-RC                     PIC  X(003).
               88 PO-DRKI2994-RC-OK               VALUE 'OK '.
               88 PO-DRKI2994-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2994-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2994-DKZAPANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2994-DKZAPEMAIL.
                49 PO-DRKI2994-DKZAPEMAIL-LEN PIC S9(4) COMP.
                49 PO-DRKI2994-DKZAPEMAIL-TEXT PIC X(128).
             10 PO-DRKI2994-DKZAPFUNKTION.
                49 PO-DRKI2994-DKZAPFUNKTION-LEN PIC S9(4) COMP.
                49 PO-DRKI2994-DKZAPFUNKTION-TEXT PIC X(50).
             10 PO-DRKI2994-DKZAPGEBDAT   PIC  X(010).
             10 PO-DRKI2994-DKZAPNACHNAME   PIC  X(0030).
             10 PO-DRKI2994-DKZAPTITEL-KNR   PIC S9(4) COMP.
             10 PO-DRKI2994-DKZAPVORNAME   PIC  X(0030).
             10 PO-DRKI2994-DKZFAXNR   PIC  X(0030).
             10 PO-DRKI2994-DKZINHABER.
                49 PO-DRKI2994-DKZINHABER-LEN PIC S9(4) COMP.
                49 PO-DRKI2994-DKZINHABER-TEXT PIC X(100).
             10 PO-DRKI2994-DKZMOBILNR   PIC  X(0030).
             10 PO-DRKI2994-DKZPOSTFACH   PIC  X(0010).
             10 PO-DRKI2994-DKZPOSTFACHORT   PIC  X(0040).
             10 PO-DRKI2994-DKZPOSTFACHPLZ   PIC S9(9) COMP.
             10 PO-DRKI2994-DKZRECHTSFORM-KNR   PIC S9(4) COMP.
             10 PO-DRKI2994-DKZTELNR   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

