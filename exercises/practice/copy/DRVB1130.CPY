      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1130.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1130.CPv  $
      *
      *   Rev 2.1   May 03 2016 13:16:58   t13ex30
      *AVB01400: Projekt Lucy2 - ZWVB0065
      *
      *   Rev 2.0   Apr 14 2016 09:47:34   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1130                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1130                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. Wölflik                                       *
      *    DATUM:    14.04.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!14.04.16! 01 ! NEU                       !H. Wölflik*
240322* VLA-4369!24.03.22! 02 ! Felddefinition COMP-3     ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1130
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1130.
           05  PI-DRVB1130-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1130-DATUM-VON          PIC  X(010).
           05  PI-DRVB1130-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1130.
           05  PO-DRVB1130-RC                 PIC  X(003).
               88 PO-DRVB1130-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1130-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1130-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1130-RC-ERR                       VALUE 'ERR'.
      *
240322     05  PO-DRVB1130-EH-NEU-GS-EG       PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

