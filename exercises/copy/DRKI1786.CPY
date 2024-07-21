      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.04.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1786.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1786.CPv  $
      *
      *   Rev 2.1   Aug 20 2018 08:06:52   t13ae31
      *AKI01619: SUH KSDS VSAM in DB2 BMG OZP
      *
      *   Rev 2.0   Apr 20 2005 13:00:44   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1786                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1786                    *
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
      *    FÜR DIE FUNKTION DRKI1786
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1786.
           05 PI-DRKI1786-PROGRAMM                PIC  X(008).
           05 PI-DRKI1786-UFKT-CODE               PIC  9(002).
 
           05  PI-DRKI1786-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI1786-G10SUHBMGVNR   PIC  X(0015).
END   *
      *
       01  PO-DRKI1786.
           05  PO-DRKI1786-RC                     PIC  X(003).
               88 PO-DRKI1786-RC-OK               VALUE 'OK '.
               88 PO-DRKI1786-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI1786-RC-EOF              VALUE 'EOF'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI1786-DATEN.
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI1786-G10SUHBMGVNR  PIC  X(0015).
             10 PO-DRKI1786-G10SUHBMGKZ   PIC  X(0001).
             10 PO-DRKI1786-G10SUHBMGTEXT   PIC  X(0030).
END   *
      *
      ******* ENDE COPY-MEMBER ***************************************

