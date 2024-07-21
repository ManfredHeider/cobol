      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3223.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3223.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 13:55:24   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 08:36:44   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 7 2017 13:51:54 T13AC14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3223                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3223                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.02.2018 / 10:38:32 / T13AC14
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
      *    FÜR DIE FUNKTION DRKI3223
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3223.
           05  PI-DRKI3223-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3223-LPRVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3223.
           05  PO-DRKI3223-RC                     PIC  X(003).
               88 PO-DRKI3223-RC-OK               VALUE 'OK '.
               88 PO-DRKI3223-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

