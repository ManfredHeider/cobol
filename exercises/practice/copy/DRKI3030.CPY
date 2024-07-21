      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3030.CPv  $
      *
      *   Rev 2.1   Sep 03 2015 09:58:38   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Up
      *dates und Deletes gangbar machen
      *
      *   Rev 2.0   Jul 16 2015 07:25:54   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3030                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3030                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01251!16.07.15!1.0 ! neu                       ! tfl      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3030
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3030.
           05 PI-DRKI3030-PROGRAMM                PIC  X(008).
           05 PI-DRKI3030-UFKT-CODE               PIC  9(002).
              88 PO-DRKI3030-UFKT-FIRST           VALUE 1.
              88 PO-DRKI3030-UFKT-NEXT            VALUE 2.
              88 PO-DRKI3030-UFKT-QUIT            VALUE 3.
 
 
           05 PI-DRKI3030-DATEN.
              10 PI-DRKI3030-G68KVERTRAGSNR       PIC  X(015).
 
 
       01  PO-DRKI3030.
           05 PO-DRKI3030-RC                      PIC  X(003).
              88 PO-DRKI3030-RC-OK                VALUE 'OK '.
              88 PO-DRKI3030-RC-EOF               VALUE 'EOF'.
              88 PO-DRKI3030-RC-LEER              VALUE 'LER'.
 
           05 PO-DRKI3030-DATEN.
              10 PO-DRKI3030-G68KLFDNR            PIC S9(004) COMP.
              10 PO-DRKI3030-G68KNACHNAME         PIC  X(030).
              10 PO-DRKI3030-G68KVORNAME          PIC  X(030).
              10 PO-DRKI3030-G68KGEBURTSDATUM     PIC  X(010).
              10 PO-DRKI3030-G68KANREDE           PIC  X(001).
              10 PO-DRKI3030-G68KGESCHLECHT       PIC  X(001).
              10 PO-DRKI3030-G68KSTRASSE          PIC  X(040).
              10 PO-DRKI3030-G68KPOSTLEITZAHL     PIC  X(009).
              10 PO-DRKI3030-G68KORT              PIC  X(030).
              10 PO-DRKI3030-G68KLAND             PIC  X(0003).
              10 PO-DRKI3030-G68KLOESCHKZ         PIC  X(001).
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 
 

