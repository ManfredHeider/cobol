      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3645.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3645.CPv  $
      *
      *   Rev 2.2   Jun 05 2019 11:13:18   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.1   Apr 15 2019 11:25:54   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.0   Mar 22 2019 15:42:55 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3645                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3645                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.03.2019 / 15:42:55 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3645
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3645.
           05  PI-DRKI3645-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3645-G67VERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3645.
           05  PO-DRKI3645-RC                     PIC  X(003).
               88 PO-DRKI3645-RC-OK               VALUE 'OK '.
               88 PO-DRKI3645-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3645-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3645-G67ABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI3645-G67DLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3645-G67FAELLIGKEIT   PIC  X(0008).
             10 PO-DRKI3645-G67KREDITART   PIC  X(0006).
             10 PO-DRKI3645-G67KREDITAUSZ   PIC  X(0008).
             10 PO-DRKI3645-G67KREDITZUS   PIC  X(0008).
             10 PO-DRKI3645-G67MANDANT   PIC S9(9) COMP.
             10 PO-DRKI3645-G67RATE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3645-G67RESTLAUFZEIT   PIC  X(0003).
             10 PO-DRKI3645-G67SALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3645-G67SICHERHEIT   PIC  X(0001).
             10 PO-DRKI3645-G67STATUS   PIC  X(0001).
             10 PO-DRKI3645-G67VRTRBEGINN   PIC  X(0008).
             10 PO-DRKI3645-G67VRTRENDE   PIC  X(0008).
             10 PO-DRKI3645-G67ZINSSATZ   PIC S9(2)V9(4) COMP-3.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


