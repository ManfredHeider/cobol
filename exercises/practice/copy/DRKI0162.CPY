      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0162.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0162.CPv  $
      *
      *   Rev 2.1   Aug 20 2018 08:06:46   t13ae31
      *AKI01619: SUH KSDS VSAM in DB2 BMG OZP
      *
      *   Rev 1.1   19 May 1998 09:34:28   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:53:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0162                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0162                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.08.2018 / 12:37:27 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0162
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0162.
           05  PI-DRKI0162-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0162-G10SUHBMGVNR   PIC  X(0015).
END   *
 
           05  PI-DRKI0162-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI0162-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI0162-G10SUHBMGKZ   PIC  X(0001).
             10 PI-DRKI0162-G10SUHBMGTEXT   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0162-NI1.
           05 PI-DRKI0162-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI0162-NI7.
           05 PI-DRKI0162-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI0162.
           05  PO-DRKI0162-RC                     PIC  X(003).
               88 PO-DRKI0162-RC-OK               VALUE 'OK '.
               88 PO-DRKI0162-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

