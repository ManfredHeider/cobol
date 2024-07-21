*****************************************************************
      *    COPY-MEMBER: DRVB1360                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.THURMANN                                       *
      *    DATUM:    24.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-4870 |24.08.22| NEU                           | THURMANN  *
      *          |        |                               |           *
      *          |        |                               |           *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1360 - ERMITTELN OFFENE ANSPRÜCHE AUS WTAUS   * 
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1360.
           05  PI-DRVB1360-DATEN.
               10  PI-DRVB1360-UFKT-CODE         PIC  9(002).
               10  PI-DRVB1360-KNR-GESELL-TYP    PIC S9(004) COMP.      
               10  PI-DRVB1360-KNR-ANSPR-STATUS  PIC S9(004) COMP.      
               10  PI-DRVB1360-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB1360-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB1360.
           05  PO-DRVB1360-RC                    PIC  X(003).
               88  PO-DRVB1360-RC-OK                  VALUE 'OK '.      
               88  PO-DRVB1360-RC-EOF                 VALUE 'EOF'.      
           05  PO-DRVB1360-DATEN.
               10  PO-DRVB1360-MA-ST-NR          PIC S9(009) COMP.      
               10  PO-DRVB1360-WTAUS-ANSPR-VOM   PIC  X(010).
               10  PO-DRVB1360-KNR-ANSPR-GRUND   PIC S9(004) COMP.      
               10  PO-DRVB1360-KTY-ANSPR-GRUND   PIC S9(004) COMP.      
               10  PO-DRVB1360-WTAUS-LAST-UPDATE PIC  X(026).
               10  PO-DRVB1360-WTAUS-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB1360-KTY-ANSPR-STATUS  PIC S9(004) COMP.      
               10  PO-DRVB1360-KNR-ANSPR-STATUS  PIC S9(004) COMP.      
      *
      ****
      **** ENDE COPY DRVB1360 ****************************************