      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0115.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0115.CPv  $
      *
      *   Rev 2.2   Sep 29 2010 11:01:48   t13ex48
      *AKI00966: Einführung DVAG Wien:-OZP TICKETDTAWIEN
      *
      *   Rev 2.1   Sep 28 2010 14:40:38   t13ex48
      *AKI00966: Einführung DVAG Wien:-OZP TICKETDTAWIEN
      *
      *   Rev 2.0   Sep 28 2010 14:24:07 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0115                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0115                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.09.2010 / 09:31:23 / T13EX48
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
      *    FÜR DIE FUNKTION DROK0115
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0115.
           05  PI-DROK0115-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DROK0115-TDWLFDNR   PIC S9(4) COMP.
             10 PI-DROK0115-TDWTICKETID   PIC S9(18)V COMP-3.
END   *


           05  PI-DROK0115-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DROK0115-TDWDTADATEN.
                49 PI-DROK0115-TDWDTADATEN-LEN PIC S9(4) COMP.
                49 PI-DROK0115-TDWDTADATEN-TEXT PIC X(400).
             10 PI-DROK0115-TDWTICKETTYP   PIC S9(4) COMP.
END   *

      *
       01  PO-DROK0115.
           05  PO-DROK0115-RC                     PIC  X(003).
               88 PO-DROK0115-RC-OK                  VALUE 'OK '.
               88 PO-DROK0115-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
