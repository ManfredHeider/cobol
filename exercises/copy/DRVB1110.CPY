      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1110.CPv  $
      *
      *   Rev 2.1   Jul 10 2015 08:52:28   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI/2 
      *
      *   Rev 2.0   Jun 16 2015 13:09:20   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1110                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1110                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    01.07.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!01.07.15!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1110
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1110.
           05  PI-DRVB1110-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB1110-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB1110-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
      *
       01  PO-DRVB1110.
           05  PO-DRVB1110-RC                 PIC  X(003).
               88 PO-DRVB1110-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1110-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1110-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1110-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1110-PARTEH-TUPEL        PIC  X(094).
      *
      ******* ENDE COPY-MEMBER **************************************



