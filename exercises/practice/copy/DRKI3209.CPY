      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3209.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3209.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 13:48:00   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:32:44   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 4 2017 16:38:41 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3209                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3209                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.12.2017 / 07:50:02 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3209
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3209.
           05  PI-DRKI3209-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3209-G20VVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3209.
           05  PO-DRKI3209-RC                     PIC  X(003).
               88 PO-DRKI3209-RC-OK               VALUE 'OK '.
               88 PO-DRKI3209-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

