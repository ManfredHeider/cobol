      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0160.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0160.CPv  $
      *
      *   Rev 2.1   Aug 20 2018 08:06:32   t13ae31
      *AKI01619: SUH KSDS VSAM in DB2 BMG OZP
      *
      *   Rev 1.1   14 May 1998 10:34:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:53:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0160                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0160                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.08.2018 / 12:37:24 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0160
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0160.
           05  PI-DRKI0160-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0160-G10SUHBMGVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI0160.
           05  PO-DRKI0160-RC                     PIC  X(003).
               88 PO-DRKI0160-RC-OK               VALUE 'OK '.
               88 PO-DRKI0160-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI0160-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI0160-G10SUHBMGKZ   PIC  X(0001).
             10 PO-DRKI0160-G10SUHBMGTEXT   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

