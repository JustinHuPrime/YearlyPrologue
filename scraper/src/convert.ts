import fs from 'fs';
import path from 'path';

const dateMap = {
  Mon: 'monday',
  Tue: 'tuesday',
  Wed: 'wednesday',
  Thu: 'thursday',
  Fri: 'friday',
  Sat: 'saturday',
  Sun: 'sunday',
}

function getIntervals(section: any): string {
  const { 
    section_terms: terms,
    section_days: days,
    section_start: start,
    section_end: end,
  } = section;

  const intervals = [];

  terms.forEach((term: number) => {
    days.forEach((day: string) => {
      const [sH, sM] = start.split(':');
      const [eH, eM] = end.split(':');
      const weekday = dateMap[day];

      if (sH === undefined) return;
      if (sM === undefined) return;
      if (eH === undefined) return;
      if (eM === undefined) return;
      if (weekday === undefined) return;

      const interval = `interval(${term}, ${weekday}, time(${sH}, ${sM}), time(${eH}, ${eM}))`;
      intervals.push(interval);
    });
  });

  if (intervals.length === 0) {
    terms.forEach((term: number) => {
      const interval = `interval(${term}, doesntmeet, time(00, 00), time(00, 00))`;
      intervals.push(interval);
    });
  }

  return `[${intervals.join(', ')}]`;
}

function main() {
  const filePath = path.join(__dirname, '../out/raw.json');
  const rawData = fs.readFileSync(filePath, { encoding: 'utf8' });
  const data = JSON.parse(rawData);

  let output = ':- discontiguous section/3, course/3.\n\n';
  output += '% YEARLY PROLOGUE COURSE DATA\n\n';

  data.courses.forEach((course: any) => {
    const { dept_code, course_number } = course;
    const courseId = `${dept_code.toLowerCase()}${course_number}`;

    output += `% ${dept_code} ${course_number}\n`;

    const { course_name } = course;
    output += `course(${courseId}, name, "${course_name}").\n`;

    const { course_credits } = course;
    output += `course(${courseId}, credits, ${course_credits}).\n`;

    const { sections } = course;
    const requiredSectionTypes = new Set();

    sections.forEach((section: any) => {
      const { section_type } = section;
      if (section.section_type === 'Waiting List') return;
      requiredSectionTypes.add(section_type);
    });

    output += `course(${courseId}, requiredSections, [${Array.from(requiredSectionTypes).map(t => `"${t}"`).join(', ')}]).\n`;

    output += '\n';

    sections.forEach((section: any) => {
        if (section.section_type === 'Waiting List') return;
        if (section.section_number === "W-L") return;

        const { section_number } = section;
        const sectionId = `${courseId}${section_number}`;

        output += `% ${dept_code} ${course_number} - ${section_number}\n`;

        output += `section(${sectionId}, course, ${courseId}).\n`;
        output += `section(${sectionId}, number, "${section_number}").\n`;

        const { section_type } = section;
        output += `section(${sectionId}, type, "${section_type}").\n`;

        const intervals = getIntervals(section);
        output += `section(${sectionId}, times, ${intervals}).\n`;

        output += '\n';
    });

    output += '\n';
  });

  const plFilePath = path.join(__dirname, '../out/course_data.pl');
  fs.writeFileSync(plFilePath, output);
}

main();
