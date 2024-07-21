      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2148.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2148.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 12:06:52   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   28 Sep 2007 16:47:44   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2148                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2148                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/11:22:33
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
      *    FÜR DIE FUNKTION DRKI2148
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2148.
           05  PI-DRKI2148-KEY.


             10 PI-DRKI2148-VRARISIKOHOSTID   PIC S9(9) COMP.
             10 PI-DRKI2148-VRAVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2148.
           05  PO-DRKI2148-RC                 PIC  X(003).
               88 PO-DRKI2148-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2148-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2148-DATEN.


             10 PO-DRKI2148-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2148-VRARISIKOLKZ   PIC S9(9) COMP.
             10 PO-DRKI2148-VRARISIKOORT   PIC X(40).
             10 PO-DRKI2148-VRARISIKOPLZ   PIC X(6).
             10 PO-DRKI2148-VRARISIKOSTRASSE   PIC X(40).
             

      *
      ******* ENDE COPY-MEMBER **************************************
