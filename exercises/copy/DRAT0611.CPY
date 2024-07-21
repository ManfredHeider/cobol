      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.12.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0611.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0611.CPv  $
      *
      *   Rev 2.1   Jan 25 2017 10:11:54   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren
      *
      *   Rev 2.0   Dec 21 2016 11:47:57 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0611                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0611                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0611
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0611.
           05  PI-DRAT0611-KEY.

             10 PI-DRAT0611-FR-FEHLERNUMMER   PIC S9(4) COMP.
             10 PI-DRAT0611-FR-FELDNAME   PIC  X(0025).
             10 PI-DRAT0611-FR-JOBNAME   PIC  X(0008).
             10 PI-DRAT0611-FR-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0611-FR-SYSCODE   PIC  X(0002).
      *

      *
       01  PO-DRAT0611.
           05  PO-DRAT0611-RC                     PIC  X(003).
               88 PO-DRAT0611-RC-OK               VALUE 'OK '.
               88 PO-DRAT0611-RC-NOT-FOUND        VALUE 'NFD'.

           05  PO-DRAT0611-DATEN.
             10 PO-DRAT0611-WERT.
               49 PO-DRAT0611-WERT-LEN           PIC S9(004) COMP.
               49 PO-DRAT0611-WERT-TEXT          PIC  X(001).
             10 PO-DRAT0611-FR-REAKTION          PIC  X(0001).
             10 PO-DRAT0611-FR-REAKTIONHINWEIS   PIC  X(0025).
             10 PO-DRAT0611-FR-RELEVANZ          PIC  X(0001).
      *
      *
      ******* ENDE COPY-MEMBER ***************************************

