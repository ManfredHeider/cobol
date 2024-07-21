      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2145.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2145.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 12:06:18   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   28 Sep 2007 14:46:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2145                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2145                    *
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
      *    FÜR DIE FUNKTION DRKI2145
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2145.
           05  PI-DRKI2145-KEY.


             10 PI-DRKI2145-VRARISIKOHOSTID   PIC S9(9) COMP.
             10 PI-DRKI2145-VRAVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2145-DATEN.


             10 PI-DRKI2145-VRARISIKOLKZ   PIC S9(9) COMP.
             10 PI-DRKI2145-VRARISIKOORT   PIC X(40).
             10 PI-DRKI2145-VRARISIKOPLZ   PIC X(6).
             10 PI-DRKI2145-VRARISIKOSTRASSE   PIC X(40).
             
      *
       01  PO-DRKI2145.
           05  PO-DRKI2145-RC                 PIC  X(003).
               88 PO-DRKI2145-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2145-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
