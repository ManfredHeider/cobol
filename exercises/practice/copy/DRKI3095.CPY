      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3095.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3095.CPv  $
      *
      *   Rev 2.1   Dec 07 2016 10:50:12   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 20 2016 14:14:28 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3095                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3095                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.11.2016 / 10:39:25 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3095
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3095.
           05  PI-DRKI3095-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3095-KVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3095-KVPVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3095.
           05  PO-DRKI3095-RC                     PIC  X(003).
               88 PO-DRKI3095-RC-OK               VALUE 'OK '.
               88 PO-DRKI3095-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

