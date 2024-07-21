      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3360.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3360.CPv  $
      *
      *   Rev 2.2   Sep 13 2018 09:57:38   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.1   Sep 10 2018 09:49:24   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.0   Jul 6 2018 09:30:11 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3360                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3360                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.07.2018 / 09:19:47 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI3360
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3360.
           05  PI-DRKI3360-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3360-G48KKAART   PIC  X(0003).
             10 PI-DRKI3360-G48KKANR   PIC  X(0016).
             10 PI-DRKI3360-G48KKAVNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3360.
           05  PO-DRKI3360-RC                     PIC  X(003).
               88 PO-DRKI3360-RC-OK               VALUE 'OK '.
               88 PO-DRKI3360-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

