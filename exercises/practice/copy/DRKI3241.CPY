      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3241.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3241.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 14:04:08   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:37:58   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 13 2017 15:08:33 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3241                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3241                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.02.2018 / 16:10:46 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3241
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3241.
           05  PI-DRKI3241-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3241-G20KROLLE   PIC  X(0004).
             10 PI-DRKI3241-G20KVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3241.
           05  PO-DRKI3241-RC                     PIC  X(003).
               88 PO-DRKI3241-RC-OK               VALUE 'OK '.
               88 PO-DRKI3241-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

