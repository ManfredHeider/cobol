      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1524.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1524.CPv  $
      *
      *   Rev 2.2   Apr 29 2009 14:58:18   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 2.0   Oct 16 2002 08:33:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1524                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1524                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.04.2009/15:54:27
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
      *    FÜR DIE FUNKTION DRKI1524
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1524.
           05  PI-DRKI1524-KEY.


             10 PI-DRKI1524-VRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1524-VRAVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1524-DATEN.


             10 PI-DRKI1524-VRARABEINHEIT   PIC S9(4) COMP.
             10 PI-DRKI1524-VRARABTEXT   PIC X(26).
             10 PI-DRKI1524-VRARABWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1524-VRARABWERWKZ   PIC X(01).
             

      *
       01  PO-DRKI1524.
           05  PO-DRKI1524-RC                 PIC  X(003).
               88 PO-DRKI1524-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1524-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
