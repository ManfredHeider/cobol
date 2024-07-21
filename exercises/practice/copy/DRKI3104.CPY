      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3104.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3104.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:16:36   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:51:48   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 13:35:12 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3104                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3104                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.12.2016 / 13:11:04 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO      *
      *         !        !    !   - G51PABRECHDAT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3104
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3104.
           05  PI-DRKI3104-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3104-G51PLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3104-G51PVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3104.
           05  PO-DRKI3104-RC                     PIC  X(003).
               88 PO-DRKI3104-RC-OK               VALUE 'OK '.
               88 PO-DRKI3104-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3104-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3104-G51PABRECHKZ   PIC  X(0001).
             10 PO-DRKI3104-G51PANREDE   PIC S9(4) COMP.
             10 PO-DRKI3104-G51PGEBDAT   PIC  X(0008).
             10 PO-DRKI3104-G51PGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3104-G51PNACHNAME   PIC  X(0030).
             10 PO-DRKI3104-G51PSTATUS   PIC  X(0001).
             10 PO-DRKI3104-G51PVERSART   PIC  X(0001).
             10 PO-DRKI3104-G51PVORNAME   PIC  X(0030).
             10 PO-DRKI3104-G51PVRTRGBEGINN   PIC  X(0008).
             10 PO-DRKI3104-G51PVRTRGENDE   PIC  X(0008).
             10 PO-DRKI3104-G51PZUSRAHMEN   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

