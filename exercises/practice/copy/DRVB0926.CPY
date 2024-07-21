      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0926.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0926.CPv  $
      *
      *   Rev 2.1   Feb 10 2009 15:06:12   t13ae27
      *AAS00796: Eliteclub Unternehmensaufbau 2009
      *
      *   Rev 2.0   Jan 06 2009 14:46:10   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0926                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0926                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    23.02.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!23.02.01!    ! NEU                       ! P. Dürr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0926
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0926.
           05  PI-DRVB0926-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0926-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0926-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
      *    05  PI-DRVB0926-KAND-WERTUNGSSTUFE PIC  S9(004)    COMP.
      *
       01  PO-DRVB0926.
           05  PO-DRVB0926-RC                 PIC  X(003).
               88 PO-DRVB0926-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0926-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0926-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0926-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0926-KAND-TUPEL-GESAMT   PIC X(119).
      *
      ******* ENDE COPY-MEMBER **************************************


