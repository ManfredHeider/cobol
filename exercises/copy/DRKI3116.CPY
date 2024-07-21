      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3116.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3116.CPv  $
      *
      *   Rev 2.1   Dec 07 2016 10:52:32   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Nov 30 2016 14:22:52 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3116                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3116                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.11.2016 / 14:22:52 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!30.11.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3116
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3116.
           05  PI-DRKI3116-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3116-KVPVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3116.
           05  PO-DRKI3116-RC                     PIC  X(003).
               88 PO-DRKI3116-RC-OK               VALUE 'OK '.
               88 PO-DRKI3116-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3116-DATEN.
             10   PO-DRKI3116-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

