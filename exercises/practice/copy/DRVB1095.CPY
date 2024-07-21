      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1095.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1095.CPv  $
      *
      *   Rev 2.1   Jun 03 2015 12:41:46   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.0   Apr 20 2015 11:09:20   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1095                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1095                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    13.04.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!13.04.15!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1095
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1095.
           05  PI-DRVB1095-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB1095-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB1095-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB1095-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
      *
       01  PO-DRVB1095.
           05  PO-DRVB1095-RC                 PIC  X(003).
               88 PO-DRVB1095-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1095-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1095-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1095-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1095-PARTEH-TUPEL        PIC  X(094).
      *
      ******* ENDE COPY-MEMBER **************************************


