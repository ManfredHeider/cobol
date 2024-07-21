      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0416.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0416.CPv  $
      *
      *   Rev 2.1   Jun 10 1999 12:09:28   T13AE28
      *AAT00011: Neue Funktion für UPVB0104, Grenze50
      *
      *   Rev 2.0   May 27 1999 13:58:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0341                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0416                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                        *
      *    DATUM:    10.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !10.06.99! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0416
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 6stelligen Jahrszahl des
      *            Formats JJMMTT in eine 6stellige Jahreszahl des
      *            mit sliding Grenze 50
      *                ------- ---------
      ****************************************************************
      *
       01  PI-DRVB0416.
           05  PI-DRVB0416-JAHR-1             PIC  9(006).
      *
       01  PO-DRVB0416.
           05  PO-DRVB0416-RC                 PIC  X(003).
               88 PO-DRVB0416-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0416-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0416-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0416-JAHR-1             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
