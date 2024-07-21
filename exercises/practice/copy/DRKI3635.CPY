      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3635.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3635.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:19:42   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:42:38   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 18 2019 11:12:17 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3635                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3635                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.03.2019 / 11:12:17 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3635
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI3635.
           05  PI-DRKI3635-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3635-G67TVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI3635-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3635-G67TANREDE   PIC  X(0001).
             10 PI-DRKI3635-G67TGEBDAT   PIC  X(0010).
             10 PI-DRKI3635-G67TLAND   PIC  X(0003).
             10 PI-DRKI3635-G67TNACHNAME   PIC  X(0030).
             10 PI-DRKI3635-G67TORT   PIC  X(0030).
             10 PI-DRKI3635-G67TPLZ   PIC  X(0009).
             10 PI-DRKI3635-G67TSTRHNR   PIC  X(0040).
             10 PI-DRKI3635-G67TTICKETID   PIC S9(18)V COMP-3.
             10 PI-DRKI3635-G67TVORNAME   PIC  X(0030).
END   *
 
      *
       01  PO-DRKI3635.
           05  PO-DRKI3635-RC                     PIC  X(003).
               88 PO-DRKI3635-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3635-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI3635-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3635-G67TLFDNR   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER **************************************


