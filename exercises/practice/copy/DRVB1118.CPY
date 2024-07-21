      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1118.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1118.CPv  $
      *
      *   Rev 2.1   Oct 13 2015 16:52:04   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.0   Sep 03 2015 14:21:02   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1118                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1118                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.Pretsch                                        *
      *    DATUM:    03.09.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!03.09.15! 01 ! NEU                       !B.Pretsch *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1118
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1118.
           05  PI-DRVB1118-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1118-DATUM-VON          PIC  X(010).
           05  PI-DRVB1118-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1118.
           05  PO-DRVB1118-RC                 PIC  X(003).
               88 PO-DRVB1118-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1118-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1118-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1118-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1118-PV-GS              PIC  S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

