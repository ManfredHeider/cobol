      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0418.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0418.CPv  $
      *
      *   Rev 2.1   Jun 30 1999 16:50:16   T13AE28
      *AAT00011: Neue Funktion für KI; Erm. GEBDAT JJJJMMTT
      *
      *   Rev 2.0   May 27 1999 13:58:36   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0418                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0418                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    30.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !30.06.99! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0418
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 6stelligen Jahrszahl des
      *            Formats JJMMTT in eine 8stellige Jahreszahl des
      *            Formats JJJJMMTT
      *            mit sliding Grenze 05
      *                ------- ---------
      ****************************************************************
      *
       01  PI-DRVB0418.
           05  PI-DRVB0418-JAHR-1             PIC  9(006).
      *
       01  PO-DRVB0418.
           05  PO-DRVB0418-RC                 PIC  X(003).
               88 PO-DRVB0418-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0418-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0418-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0418-JAHR-1             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************

