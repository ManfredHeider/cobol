      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1153.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1153.CPv  $
      *
      *   Rev 2.1   Nov 02 2016 09:52:20   t13ex30
      *ASL00559: Betreuung Versorgungsleistungen 2016 - PMSL0068 un
      *d PMSL0109 (HWO)
      *
      *   Rev 2.0   Oct 12 2016 11:54:58   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1153                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1153                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    12.10.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00559!12.10.16!    ! NEU                       ! H.WÖLFLIK*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1153
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1153.
           10 PI-DRVB1153-BES-VB-NR           PIC  S9(009) COMP.
           10 PI-DRVB1153-BES-KTY-NR          PIC  S9(004) COMP.
           10 PI-DRVB1153-BES-KNR-NR          PIC  S9(004) COMP.
           10 PI-DRVB1153-BES-ZEITPUNKT       PIC  X(010).
      *
       01  PO-DRVB1153.
           05  PO-DRVB1153-RC                 PIC  X(003).
               88 PO-DRVB1153-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1153-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1153-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1153-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB1153-BESCHRAENKUNG-JA            VALUE 'J'.
               88  PO-DRVB1153-BESCHRAENKUNG-NEIN          VALUE 'N'.
      ******* ENDE COPY-MEMBER **************************************
 

