      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1158.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1158.CPv  $
      *
      *   Rev 2.1   Feb 13 2017 09:49:38   t13ae13
      *AVB01422: neuer VB-Vertrag 2017: UPVB0128 anpassen (SWL)
      *
      *   Rev 2.0   Jan 24 2017 12:10:32   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1158                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1158                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01422!24.01.17!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1158
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1158.
           05  PI-DRVB1158-KEY.
               10 PI-DRVB1158-MA-ST-NR        PIC S9(009) COMP.
               10 PI-DRVB1158-STICHTAG-ISO    PIC  X(010).
      *
       01  PO-DRVB1158.
           05  PO-DRVB1158-RC                 PIC  X(003).
               88 PO-DRVB1158-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1158-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1158-AV-DAT-VON         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************


