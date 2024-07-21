      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3236.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3236.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 14:01:36   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 09:14:30   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 11 2017 13:42:54 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3236                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3236                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.02.2018 / 11:09:59 / T13AC17
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
      *    FÜR DIE FUNKTION DRKI3236
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3236.
           05  PI-DRKI3236-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3236-LPTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3236-LPTVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3236.
           05  PO-DRKI3236-RC                     PIC  X(003).
               88 PO-DRKI3236-RC-OK               VALUE 'OK '.
               88 PO-DRKI3236-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

